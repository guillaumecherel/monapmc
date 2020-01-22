{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}

module ABC.Lenormand2012 where

import Protolude

import qualified Data.List as List
import Control.Monad.Random.Lazy
import Control.Monad.Zip
import qualified Numeric.LinearAlgebra as LA
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
  { t :: !Int
  , thetas:: !(LA.Matrix Double)
  , weights:: !(LA.Vector Double)
  , ts :: !(V.Vector Int)
  , rhos:: !(LA.Vector Double)
  , pAcc:: !Double
  , epsilon:: !Double
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
  thetasV <- stepOnePre p
  xsV <- traverse f thetasV
  stepOnePost p thetasV xsV


stepOnePre :: (Monad m) => P m -> m [V.Vector Double]
stepOnePre p = sequence $ replicate (n p) (priorSample p)

stepOnePost
  :: (Monad m) => P m -> [V.Vector Double] -> [V.Vector Double] -> m S
stepOnePost p thetasV xsV = do
  let newThetas = LA.fromLists $ fmap V.toList thetasV
  let xs = LA.fromLists $ fmap V.toList xsV
  let dim = LA.cols (newThetas :: LA.Matrix Double)
  let obs = LA.vector $ V.toList $ observed p
  let newRhos = LA.cmap sqrt $
               ((xs - LA.asRow obs) ** 2) LA.#> LA.konst 1 dim
  let (select, rhoSelected) = second LA.fromList $ unzip
                              $ take (nAlpha p)
                              $ sortOn snd
                              $ zip [0..] (LA.toList newRhos)
  let newEpsilon = rhoSelected LA.! (LA.size rhoSelected - 1)
  let thetaSelected = newThetas LA.? select
  let newPAcc = 1
  let weightsSelected = LA.konst 1 (LA.size rhoSelected)
  let tsSelected = V.fromList $ replicate (LA.size rhoSelected) 1
  return $ S {t = 1, thetas = thetaSelected, weights = weightsSelected, ts = tsSelected ,rhos = rhoSelected, pAcc = newPAcc, epsilon = newEpsilon}

step
  :: (MonadRandom m)
  => P m
  -> (V.Vector Double -> m (V.Vector Double))
  -> S
  -> m S
step p f s = do
  (sigmaSquared, newThetas) <- stepPre p s
  -- TODO: check performance wrapping/unwrapping
  newXs <- LA.fromRows . fmap (LA.fromList . V.toList) <$> traverse (f . V.fromList . LA.toList) (LA.toRows newThetas)
  stepPost p s (sigmaSquared, newThetas) newXs

stepPre
  :: (MonadRandom m)
  => P m
  -> S
  -> m (LA.Herm Double, LA.Matrix Double)
stepPre p s = do
  resampleIndices <- replicateM (n p - nAlpha p) $ weighted $ (mzip [0..] (fmap toRational $ LA.toList $ weights s))
  let resampleThetas = thetas s LA.? resampleIndices
  seed <- getRandom
  let dim = LA.cols (thetas s)
  let sigmaSquared = LA.scale 2
                         $ weightedCovariance (thetas s) (weights s)
  let newThetas =  resampleThetas +
                    LA.gaussianSample seed (n p - nAlpha p)
                      (LA.konst 0 dim)
                      sigmaSquared
  return (sigmaSquared, newThetas)

stepPost
  :: (MonadRandom m)
  => P m
  -> S
  -> (LA.Herm Double, LA.Matrix Double)
  -> LA.Matrix Double
  -> m S
stepPost p s (sigmaSquared, newThetas) newXs = do
  let obs = LA.vector $ V.toList $ observed p
  let dim = LA.cols (thetas s)
  let newRhos = LA.cmap sqrt $
               ((newXs - LA.asRow obs) ** 2) LA.#> LA.konst 1 dim
  let ((selectPrev, prevRhosSelected), (selectNew, newRhosSelected)) =
          bimap
              (second LA.fromList . unzip . fmap snd)
              (second LA.fromList . unzip . fmap snd)
            $ List.partition (\(w,_) -> w == (1::Int))
            $ take (nAlpha p)
            $ sortOn (snd . snd)
            $ (fmap (1,) (zip [0..] $ LA.toList $ rhos s)
               <> fmap (2,) (zip [0..] $ LA.toList $ newRhos))
  let rhosSelected = LA.vjoin [prevRhosSelected, newRhosSelected]
  let newEpsilon = rhosSelected LA.! (nAlpha p - 1)
  let newPAcc = fromIntegral (LA.size newRhosSelected) / fromIntegral (n p - nAlpha p)
  let prevThetasSelected = thetas s LA.? selectPrev
  let newThetasSelected = newThetas LA.? selectNew
  let thetasSelected = prevThetasSelected LA.=== newThetasSelected
  let prevWeightsSelected = LA.fromList $ fmap (weights s LA.!) selectPrev
  let newWeightsSelected = compWeights p s sigmaSquared newThetasSelected
  let weightsSelected = LA.vjoin [prevWeightsSelected, newWeightsSelected]
  let newT = t s + 1
  let tsSelected = V.fromList $ take (nAlpha p) (fmap (ts s V.!) selectPrev <> repeat newT)
  return $ S { t = newT
             , thetas = thetasSelected
             , weights = weightsSelected
             , ts = tsSelected
             , rhos = rhosSelected
             , pAcc = newPAcc
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
      sampleMean = (LA.scale (1 / weightsSum) weights') LA.<# sample
      sampleCenteredWeighted = (sample - LA.asRow sampleMean) * (LA.asColumn $ sqrt weights')
      (_, covCenteredWeighted) = LA.meanCov sampleCenteredWeighted
  in LA.scale ((fromIntegral n' / weightsSum) * (weightsSumSquared / (weightsSumSquared - weightsSquaredSum))) covCenteredWeighted
