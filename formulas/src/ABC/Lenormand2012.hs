{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module ABC.Lenormand2012 where

import Data.Monoid
import Control.Monad
import Control.Monad.Random.Lazy
import Control.Monad.Zip
import qualified Numeric.LinearAlgebra as LA
import qualified Statistics.Quantile as SQ
import qualified Data.Vector as V
import qualified Data.Text as T

import Debug

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
  , sigmaSquared:: LA.Herm Double
  , pAcc:: Double
  , epsilon:: Double
  } deriving (Show)
  
pprintS :: S -> T.Text
pprintS s = T.pack $ show $ thetas s

run :: (MonadRandom m) => P m -> (V.Vector Double -> m (V.Vector Double)) -> m S
run p f = stepOne p f >>= go
  where go s = do
          s' <- step p f s
          if stop s'
            then return s'
            else go s'
        stop s = pAcc s < pAccMin p

scan :: (MonadRandom m) => P m -> (V.Vector Double -> m (V.Vector Double)) -> m [S]
scan p f =
  stepOne p f >>= go
  where go s = do
          if stop s
            then return [s]
            else step p f s >>= fmap (s:) . go
        stop s = pAcc s <= pAccMin p

stepOne :: (MonadRandom m) => P m -> (V.Vector Double -> m (V.Vector Double)) -> m S
stepOne p f = do
  thetasV <- sequence $ replicate (n p) (priorSample p)
  xsV <- traverse f thetasV
  let thetas = LA.fromLists $ fmap V.toList thetasV
  let xs = LA.fromLists $ fmap V.toList xsV
  let dim = LA.cols (thetas :: LA.Matrix Double)
  let obs = LA.vector $ V.toList $ observed p
  -- TODO: euclidean distance
  let rhos = LA.cmap sqrt $
               ((xs - LA.asRow obs) ** 2) LA.#> LA.konst 1 dim
  let epsilon = SQ.weightedAvg (nAlpha p) (n p - 1) rhos
  let select = LA.find (< epsilon) rhos
  let thetaSelected = thetas LA.? select
  let rhoSelected = LA.vector $ fmap (rhos LA.!) select
  let sigmaSquared = LA.scale 2 $ snd $ LA.meanCov thetaSelected
  let pAcc = 1
  let weightsSelected = LA.konst 1 (nAlpha p)
  return $ S {thetas = thetaSelected, weights = weightsSelected, rhos = rhoSelected, sigmaSquared = sigmaSquared, pAcc = pAcc, epsilon = epsilon}

step :: (MonadRandom m) => P m -> (V.Vector Double -> m (V.Vector Double)) -> S -> m S
step p f s = do
  let nMinusNAlpha = n p - nAlpha p
  resampleIndices <- replicateM nMinusNAlpha $ weighted $ (mzip [0..] (fmap toRational $ LA.toList $ weights s))
  let resampleThetas = thetas s LA.? resampleIndices
  seed <- getRandom
  let dim = LA.cols (thetas s)
  -- TODO: check that I can take the mean out of the sample generation
  let newThetas =  resampleThetas +
                    LA.gaussianSample seed nMinusNAlpha
                      (LA.konst 0 dim)
                      (  sigmaSquared s)
  -- TODO: check performance wrapping/unwrapping
  newXs <- LA.fromRows . fmap (LA.fromList . V.toList) <$> traverse (f . V.fromList . LA.toList) (LA.toRows newThetas)
  let obs = LA.vector $ V.toList $ observed p
  let newRhos = LA.cmap sqrt $
               ((newXs - LA.asRow obs) ** 2) LA.#> LA.konst 1 dim
  let allThetas = (thetas s) LA.=== newThetas
  let allRhos = LA.vjoin [rhos s, newRhos]
  let newEpsilon = SQ.weightedAvg (nAlpha p) (n p - 1) allRhos
  let newPAcc = (1 / fromIntegral nMinusNAlpha) *
                  (LA.sumElements $ LA.step (LA.scalar newEpsilon - newRhos))
  let select = LA.find (< newEpsilon) allRhos
  let thetaSelected = allThetas LA.? select
  let rhoSelected = LA.vector $ fmap (allRhos LA.!) select
  let newWeightsSelected = compWeights p s
                            (newThetas LA.? LA.find (< newEpsilon) newRhos)
  let previousWeightsSelected = LA.vector $ fmap (weights s LA.!)
                                  (LA.find (< newEpsilon) (rhos s))
  let weightsSelected = LA.vjoin [ previousWeightsSelected
                                 , newWeightsSelected]
  let newSigmaSquared = LA.scale 2
                         $ weightedCovariance thetaSelected weightsSelected
  let newS =  S { thetas = thetaSelected
                 -- TODO: les poids décroissent trop vite
                 , weights = weightsSelected
                 , rhos = rhoSelected
                 , sigmaSquared = newSigmaSquared
                 , pAcc = newPAcc
                 , epsilon = newEpsilon
                 }
  return newS

compWeights :: P m -> S -> LA.Matrix Double -> LA.Vector Double
compWeights p s newThetasSelected =
  let weightSum = LA.sumElements (weights s)
      (inverseSigmaSquared, (lnDetSigmaSquared, signDetSigmaSquared)) = LA.invlndet $ LA.unSym (sigmaSquared s)
      sqrtDet2PiSigmaSquared = (2.0 * pi) ** (fromIntegral (LA.cols $ LA.unSym $ sigmaSquared s) / 2.0) * signDetSigmaSquared * exp (lnDetSigmaSquared / 2.0)
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
weightedCovariance sample weights =
  let n = LA.rows sample
      weightsSum :: Double
      weightsSum = LA.sumElements weights
      weightsSumSquared = weightsSum ** 2
      weightsSquaredSum = getSum $ foldMap (\x -> Sum (x ** 2)) $ LA.toList weights
      sampleMean :: LA.Vector Double
      -- MATRIX PRODUCT
      sampleMean = (LA.scale (1 / weightsSum) weights) LA.<# sample
      sampleCenteredWeighted = (sample - LA.asRow sampleMean) * (LA.asColumn $ sqrt weights)
      (_, covCenteredWeighted) = LA.meanCov sampleCenteredWeighted
  in LA.scale ((fromIntegral n / weightsSum) * (weightsSumSquared / (weightsSumSquared - weightsSquaredSum))) covCenteredWeighted 
