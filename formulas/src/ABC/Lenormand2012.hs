{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module ABC.Lenormand2012 where

import Data.Monoid
import Control.Monad
import Control.Monad.State
import Control.Monad.Random
import Control.Monad.Zip
import Data.List
import qualified Numeric.LinearAlgebra as LA
import qualified Statistics.Quantile as SQ
import System.Random
import qualified Data.Vector as V
import qualified Data.Text as T
import Debug.Trace

-- The algorithm parameters
data P m = P
  { n :: Int
  , nAlpha :: Int
  , pAccMin :: Double
  , priorSample :: m (V.Vector Double)
  , priorDensity :: V.Vector Double -> Double
  , distanceToData :: V.Vector Double -> Double
  }

-- The algorithm's state.
data S = S
  { thetas:: V.Vector (V.Vector Double)
  , weights:: V.Vector Double
  , rhos:: V.Vector Double
  , sigmaSquared:: LA.Herm Double
  , pAcc:: Double
  , epsilon:: Double
  } deriving (Show)
  
pprintS :: S -> T.Text
pprintS s = T.pack $ show $ thetas s

-- The monad in which the algorithm runs.
-- type M g a = RandT g (State S) a

-- Returns a scan over the algorithm steps, i.e.
--
-- > [stepOne, stepOne >>= step, stepOne >>= step >>= step, ...]
--
-- Uses scanl' to avoid memory leaks
lenormand2012 :: (MonadRandom m) => P m -> (V.Vector Double -> m (V.Vector Double)) -> m [S]
lenormand2012 p f = sequence $ scanl' (>>=) (stepOne p f) (repeat (step p f))

stepOne :: (MonadRandom m) => P m -> (V.Vector Double -> m (V.Vector Double)) -> m S
stepOne p f = do
  thetas <- V.replicateM (n p) (priorSample p)
  x <- traverse f thetas
  let rhos = fmap (distanceToData p) x
  let epsilon = SQ.weightedAvg (nAlpha p) (n p - 1) rhos
  let select = fmap (< epsilon) rhos
  let selected xs = fmap fst $ V.filter snd (mzip xs select)
  let thetaSelected = selected thetas
  let rhoSelected = selected rhos
  let sigmaSquared = LA.scale 2 $ snd $ LA.meanCov $ LA.fromLists $ V.toList $ fmap V.toList thetaSelected
  let pAcc = 1
  let weightsSelected = V.replicate (nAlpha p) 1
  return $ S {thetas = thetaSelected, weights = weightsSelected, rhos = rhoSelected, sigmaSquared = sigmaSquared, pAcc = pAcc, epsilon = epsilon}

step :: (MonadRandom m) => P m -> (V.Vector Double -> m (V.Vector Double)) -> S -> m S
step p f s = do
  let nMinusNAlpha = n p - nAlpha p
  resample <- replicateM nMinusNAlpha $ weighted $ (mzip (thetas s) (fmap toRational $ weights s))
  hmSeeds <- getRandoms
  let newThetas = V.fromList $ fmap (\(seed, mean) -> V.fromList $ head $ LA.toLists $ LA.gaussianSample seed 1 (LA.fromList $ V.toList mean) (sigmaSquared s)) (mzip hmSeeds resample)
  newXs <- traverse f newThetas
  let allThetas = (thetas s) <> newThetas
  let newRhos = fmap (distanceToData p) newXs
  let allRhos = (rhos s) V.++ (newRhos)
  let newEpsilon = SQ.weightedAvg (nAlpha p) (n p - 1) allRhos
  let newPAcc = (1 / fromIntegral nMinusNAlpha) * (getSum $ foldMap (\r -> if (r < newEpsilon) then Sum 1 else Sum 0) newRhos)
  let select = fmap (< newEpsilon) allRhos
  let selected xs = fmap fst $ mfilter snd (mzip xs select)
  let thetaSelected = selected allThetas
  let rhoSelected = selected allRhos
  --TODO: tous les poids sont 0 ou 1
  let newWeightsSelected = fmap (weight p s)
                                $ fmap fst $ mfilter snd
                                $ mzip newThetas (V.drop (nAlpha p) select)
  let weightsSelected = (fmap fst $ mfilter snd $ mzip (weights s) select) <> newWeightsSelected
  let newSigmaSquared = weightedCovariance (LA.fromLists $ fmap V.toList $ V.toList thetaSelected) (LA.fromList $ V.toList weightsSelected)
  let newS =  S { thetas = thetaSelected
                 , weights = weightsSelected
                 , rhos = rhoSelected
                 , sigmaSquared = newSigmaSquared
                 , pAcc = newPAcc
                 , epsilon = newEpsilon
                 }
  -- trace ("trace: " ++ show newS) $ return ()
  return newS

weight :: P m -> S -> V.Vector Double -> Double
weight p s theta =
  let weightSum = sum (weights s)
      (inverseSigmaSquared, (lnDetSigmaSquared, signDetSigmaSquared)) = LA.invlndet $ LA.unSym (sigmaSquared s)
      sqrtDet2PiSigmaSquared = (2.0 * pi) ** (fromIntegral (LA.cols $ LA.unSym $ sigmaSquared s) / 2.0) * signDetSigmaSquared * exp (lnDetSigmaSquared / 2.0)
      thetaV = LA.fromList $ V.toList theta
  in priorDensity p theta / (getSum (
        foldMap (\(wJ, thetaJ) -> Sum (
                  let thetaJV = LA.fromList $ V.toList thetaJ
                      thetaDiff = thetaV - thetaJV
                  in (wJ / weightSum)
                    * (exp $ (-0.5) * ((thetaDiff LA.<# inverseSigmaSquared) LA.<.> thetaDiff) )
                    / sqrtDet2PiSigmaSquared))
                (mzip (weights s) (thetas s))))

weightedCovariance :: LA.Matrix Double -> LA.Vector Double -> LA.Herm Double
weightedCovariance sample weights =
  let n = LA.rows sample
      weightsSum :: Double
      weightsSum = LA.sumElements weights
      weightsSumSquared = weightsSum ** 2
      weightsSquaredSum = getSum $ foldMap (\x -> Sum (x ** 2)) $ LA.toList weights
      sampleMean :: LA.Vector Double
      sampleMean = (LA.scale (1 / weightsSum) weights) LA.<# sample
      sampleCenteredWeighted = (sample - LA.asRow sampleMean) * (LA.asColumn $ sqrt weights)
      (_, covCenteredWeighted) = LA.meanCov sampleCenteredWeighted
  in LA.scale ((fromIntegral n / weightsSum) * (weightsSumSquared / (weightsSumSquared - weightsSquaredSum))) covCenteredWeighted 
