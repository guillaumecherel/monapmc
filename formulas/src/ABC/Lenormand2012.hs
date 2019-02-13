{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module ABC.Lenormand2012 where

import qualified Data.List as List
import Data.Monoid
import Control.Monad
import Control.Monad.Random.Lazy
import Control.Monad.Zip
import qualified Numeric.LinearAlgebra as LA
import qualified Statistics.Quantile as SQ
import qualified Data.Vector as V
import qualified Data.Text as T


-- TODO. Dans Beaumont2009, le facteur 2 par lequel on multiplie la variance pondérée de l'échantillon pour calculer la nouvelle variance est fait composant par composant. Il faut peut-être considérer le noyau de transition composant par composant, plutôt que de le faire sur toutes les dimensions en utilisant une matrice de covariance comme c'est fait là. Ça veut dire qu'on utilise à la place un vecteur de variance et que les covariances entre les composants des thetas ne sont pas pris en compte pour les transitions. Ça peut accélerer le calcul et réduire l'emprunte mémoire.

-- TODO. Mon implémentation est multidimensionnelle (pour theta), mais n'a
-- pas été testée avec plusieurs dimensions.

-- The algorithm parameters
-- TODO. V.Vector performance issue vs LA.Vector?
data P m = P
  { n :: Int
  , nAlpha :: Int
  , pAccMin :: Double
  , priorSample :: m (V.Vector Double)
  , priorDensity :: V.Vector Double -> Double
  , observed :: V.Vector Double
  }

-- The algorithm's state.
data S = S
  { thetas:: LA.Matrix Double
  , weights:: LA.Vector Double
  , rhos:: LA.Vector Double
  , pAcc:: Double
  , epsilon:: Double
  } deriving (Show)
  
pprintS :: S -> T.Text
pprintS s = T.pack $ show $ thetas s

run :: (MonadRandom m) => P m -> (V.Vector Double -> m (V.Vector Double)) -> m S
run p f = stepOne p f >>= go
  where go s = do
          s' <- step p f s
          if stop p s'
            then return s'
            else go s'

scan :: (MonadRandom m) => P m -> (V.Vector Double -> m (V.Vector Double)) -> m [S]
scan p f =
  stepOne p f >>= go
  where go s = do
          if stop p s
            then return [s]
            else step p f s >>= fmap (s:) . go

stop :: (Monad m) => P m -> S -> Bool
stop p s = pAcc s <= pAccMin p

stepOne :: (Monad m) => P m -> (V.Vector Double -> m (V.Vector Double)) -> m S
stepOne p f = do
  thetasV <- sequence $ replicate (n p) (priorSample p)
  xsV <- traverse f thetasV
  let newThetas = LA.fromLists $ fmap V.toList thetasV
  let xs = LA.fromLists $ fmap V.toList xsV
  let dim = LA.cols (newThetas :: LA.Matrix Double)
  let obs = LA.vector $ V.toList $ observed p
  -- TODO: euclidean distance
  let newRhos = LA.cmap sqrt $
               ((xs - LA.asRow obs) ** 2) LA.#> LA.konst 1 dim
  let newEpsilon = SQ.weightedAvg (nAlpha p) (n p - 1) newRhos
  let select = LA.find (< newEpsilon) newRhos
  let thetaSelected = newThetas LA.? select
  let rhoSelected = LA.vector $ fmap (newRhos LA.!) select
  let newPAcc = 1
  let weightsSelected = LA.konst 1 (nAlpha p)
  return $ S {thetas = thetaSelected, weights = weightsSelected, rhos = rhoSelected, pAcc = newPAcc, epsilon = newEpsilon}

step :: (MonadRandom m) => P m -> (V.Vector Double -> m (V.Vector Double)) -> S -> m S
step p f s = stepMerge p s <$> stepGen p f s

-- Generate new particles
stepGen :: (MonadRandom m) => P m -> (V.Vector Double -> m (V.Vector Double)) -> S -> m S
stepGen p f s = do
  let nMinusNAlpha = n p - nAlpha p
  resampleIndices <- replicateM nMinusNAlpha $ weighted $ (mzip [0..] (fmap toRational $ LA.toList $ weights s))
  let resampleThetas = thetas s LA.? resampleIndices
  seed <- getRandom
  let dim = LA.cols (thetas s)
  let sigmaSquared = LA.scale 2
                         $ weightedCovariance (thetas s) (weights s)
  -- TODO: check that I can take the mean out of the sample generation
  let newThetas =  resampleThetas +
                    LA.gaussianSample seed nMinusNAlpha
                      (LA.konst 0 dim)
                      sigmaSquared
  -- TODO: check performance wrapping/unwrapping
  newXs <- LA.fromRows . fmap (LA.fromList . V.toList) <$> traverse (f . V.fromList . LA.toList) (LA.toRows newThetas)
  let obs = LA.vector $ V.toList $ observed p
  let newRhos = LA.cmap sqrt $
               ((newXs - LA.asRow obs) ** 2) LA.#> LA.konst 1 dim
  let newPAcc = (1 / fromIntegral nMinusNAlpha) *
                  (LA.sumElements $ LA.step (LA.scalar (epsilon s) - newRhos))
  let select = LA.find (<= epsilon s) newRhos
  let thetaSelected = newThetas LA.? select
  let rhoSelected = LA.vector $ fmap (newRhos LA.!) select
  let weightsSelected = compWeights p s sigmaSquared thetaSelected
  return $ S { thetas = thetaSelected
             , weights = weightsSelected
             , rhos = rhoSelected
             , pAcc = newPAcc
             , epsilon = epsilon s
             }

stepMerge :: P m -> S -> S -> S
stepMerge p s s' = 
  let allThetas = thetas s LA.=== thetas s'
      allRhos = LA.vjoin [rhos s, rhos s']
      allWeights = LA.vjoin [weights s, weights s']
      newEpsilon = head $ drop (nAlpha p - 1) $ List.sort $ LA.toList allRhos
      select = LA.find (<= newEpsilon) allRhos
      thetaSelected = allThetas LA.? select
      rhoSelected = LA.vector $ fmap (allRhos LA.!) select
      weightsSelected = LA.vector $ fmap (allWeights LA.!) select
  in  S { thetas = thetaSelected
        , weights = weightsSelected
        , rhos = rhoSelected
        , pAcc = pAcc s'
        , epsilon = newEpsilon
        }

compWeights :: P m -> S -> LA.Herm Double -> LA.Matrix Double
            -> LA.Vector Double
compWeights p s sigmaSquared newThetasSelected =
  let weightSum = LA.sumElements (weights s)
      (inverseSigmaSquared, (lnDetSigmaSquared, signDetSigmaSquared)) = LA.invlndet $ LA.unSym sigmaSquared
      sqrtDet2PiSigmaSquared = (2.0 * pi) ** (fromIntegral (LA.cols $ LA.unSym sigmaSquared) / 2.0) * signDetSigmaSquared * exp (lnDetSigmaSquared / 2.0)
      normFactor thetaI = let thetaDiff = LA.asRow thetaI - thetas s
                    in LA.sumElements $
                         (weights s / LA.scalar weightSum)
                            * LA.scalar (1 / sqrtDet2PiSigmaSquared)
                            * exp (LA.scalar (-0.5) * LA.vector
                                    (zipWith (LA.<.>)
                                      (LA.toRows $
                                        thetaDiff LA.<> inverseSigmaSquared)
                                      (LA.toRows thetaDiff)))
      -- TODO: wrapping/unwrapping performance ?
      priors = LA.fromList $ fmap (priorDensity p . V.fromList)
                                 (LA.toLists newThetasSelected)
      normFactors = LA.vector $ fmap normFactor $ LA.toRows newThetasSelected
  in priors / normFactors

weightedCovariance :: LA.Matrix Double -> LA.Vector Double -> LA.Herm Double
weightedCovariance sample weights' =
  let n' = LA.rows sample
      weightsSum :: Double
      weightsSum = LA.sumElements weights'
      weightsSumSquared = weightsSum ** 2
      weightsSquaredSum = getSum $ foldMap (\x -> Sum (x ** 2)) $ LA.toList weights'
      sampleMean :: LA.Vector Double
      -- MATRIX PRODUCT
      sampleMean = (LA.scale (1 / weightsSum) weights') LA.<# sample
      sampleCenteredWeighted = (sample - LA.asRow sampleMean) * (LA.asColumn $ sqrt weights')
      (_, covCenteredWeighted) = LA.meanCov sampleCenteredWeighted
  in LA.scale ((fromIntegral n' / weightsSum) * (weightsSumSquared / (weightsSumSquared - weightsSquaredSum))) covCenteredWeighted
