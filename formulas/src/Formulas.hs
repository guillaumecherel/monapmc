{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Formulas where

import qualified Control.Foldl as L
import Data.Bool
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Statistics.Sample.Histogram as S
import qualified Statistics.Distribution.Normal as Normal
import Data.Foldable (foldl')
import qualified Data.Random as R
import Data.Monoid
import Text.Read
import Control.Monad.Random
import Control.Monad.State
import qualified System.Random.MWC as MWC
import Distribution

import Data.Word (Word64, Word32)

import ABC.Lenormand2012 (lenormand2012)

data Algorithm =
  Lenormand2012
    { getN :: Int
    , getAlpha :: Double
    , getPAccMin :: Double
    }
  | Beaumont2009
    { getN :: Int
    , getEpsilonFrom :: Double
    , getEpsilonTo :: Double
    }
  | SteadyState
    { getN :: Int
    , getAlpha :: Double
    , getPAccMin :: Double
    , getParallel :: Int
    }
  deriving (Show, Eq, Ord)

data SimulationResult = SimulationResult
  { getAlgorithm :: Algorithm
  , getStep :: Int
  , getReplication:: Int
  , getSample:: V.Vector (V.Vector Double)
  }
  deriving (Eq, Ord)

toyModelVar1 = 1 / 100
toyModelVar2 = 1

-- Mixture of 2 gaussians with equal weight, mean 0 and one has variance 0.01
-- and the other 1.
toyModel :: forall m . (MonadRandom m) => V.Vector Double -> m (V.Vector Double)
toyModel theta = do
  b <- R.runRVar R.stdUniform (getRandom :: m Word64)
  fmap V.singleton $ if b
    then normalRandomSample (V.head theta) toyModelVar1
    else normalRandomSample (V.head theta) toyModelVar2

toyPrior :: V.Vector Double -> Double
toyPrior = Distribution.uniformDensity (-10,10) . V.head

toyPriorRandomSample :: (MonadRandom m) => m (V.Vector Double)
toyPriorRandomSample = V.singleton <$> uniformRandomSample (-10,10)

toyPosterior :: Double -> Double -> Double
toyPosterior theta x =
  0.5 * normalDensity theta toyModelVar1 x + 0.5 * normalDensity theta toyModelVar2 x

toyPosteriorRegularSample :: Double -> Double -> Double -> Int -> [(Double, Double)]
toyPosteriorRegularSample theta lowerBound upperBound samples =
  let every = (upperBound - lowerBound) / fromIntegral samples
  in [(x, toyPosterior theta x) | x <- [lowerBound, lowerBound + every .. upperBound]]

histogram :: Double ->  Double -> Int -> [Double] -> [(Double, Double)]
histogram lowerBound upperBound bins xs =
  let every = (upperBound - lowerBound) / fromIntegral bins
      lowerBoundBins = [lowerBound, lowerBound + every .. upperBound]
      statHist = V.toList $ S.histogram_ bins lowerBound upperBound (V.fromList $ filter (\x -> x > lowerBound && x <= upperBound) xs)
  in zip lowerBoundBins statHist

scaledHistogram :: Double -> Double -> Int -> [Double] -> [(Double, Double)]
scaledHistogram lowerBound upperBound bins xs =
  let hist = histogram lowerBound upperBound bins xs
      binWidth = (upperBound - lowerBound) / fromIntegral bins
      scalingFactor = 1.0 / (sum (map snd hist) * binWidth)
  in map (\(x, h) -> (x, h * scalingFactor)) hist

scaledHistogramLeastSquares :: Double -> Double -> Int -> (Double -> Double)  -> [Double]-> [(Double, Double)]
scaledHistogramLeastSquares lowerBound upperBound bins targetDensity xs  = 
  let hist = histogram lowerBound upperBound bins xs
      counts = map snd hist
      binWidth = fst (head hist) - fst (head $ tail hist)
      density = map targetDensity [x + (binWidth / 2.0) | (x, _) <- hist]
      scalingFactor = foldl' (\s (c, d) -> s + c * d) 0 (zip counts density) / foldl' (\s d -> s + d ** 2) 0 counts
  in map (fmap (* scalingFactor)) hist

posteriorL2 :: Double -> Double -> Int -> (Double -> Double) -> [Double] -> Double
posteriorL2 lowerBound upperBound bins targetDensity xs =
  let scaledHist = scaledHistogram lowerBound upperBound bins xs
      binWidth = (upperBound - lowerBound) / fromIntegral bins
      density = [targetDensity (x + (binWidth / 2.0)) | (x,_) <- scaledHist]
      scaledHistHeights = map snd scaledHist
  in sqrt $ getSum $ foldMap (\(d,h) -> Sum $ (d - h) ** 2) (zip density scaledHistHeights)

numberSimus :: SimulationResult -> Int
numberSimus SimulationResult {getAlgorithm=Lenormand2012 {getN=n, getAlpha=alpha}, getStep=step} = numberSimusLenormand2012 n (floor $ fromIntegral n * alpha) step
numberSimus SimulationResult {getStep=step} = numberSimusSteadyState step

numberSimusLenormand2012 :: Int -> Int -> Int -> Int
numberSimusLenormand2012 n nAlpha step = n + (n - nAlpha) * step

numberSimusSteadyState :: Int -> Int
numberSimusSteadyState step = step

posteriorL2Mean :: Double -> Double -> Int -> (Double -> Double) -> L.Fold SimulationResult Double
posteriorL2Mean lowerBound upperBound bins density = L.premap (posteriorL2 lowerBound upperBound bins density . V.toList . fmap V.head getSample) L.mean

numberSimusMean :: L.Fold SimulationResult Double
numberSimusMean = L.premap (fromIntegral . numberSimus) L.mean

alphaMean :: L.Fold SimulationResult Double
alphaMean = L.premap (getAlpha . getAlgorithm) L.mean

groupReplications :: L.Fold SimulationResult r -> L.Fold SimulationResult (M.Map Algorithm r)
groupReplications = L.groupBy getAlgorithm

filterSimulations :: [SimulationResult -> Bool] -> L.Fold SimulationResult b -> L.Fold SimulationResult b
filterSimulations filters = L.prefilter allPass
  where allPass s = all (\f -> f s) filters

isLenormand2012 :: Algorithm -> Bool
isLenormand2012 (Lenormand2012 {}) = True
isLenormand2012 _ = False

l2VsNSimus :: Double -> Double -> Int -> (Double -> Double) -> L.Fold SimulationResult [(Double, Double)]
l2VsNSimus lowerBound upperBound bins density = M.elems <$> groupReplications ((,) <$> numberSimusMean <*> posteriorL2Mean lowerBound upperBound bins density )
  
l2VsAlpha :: Double -> Double -> Int -> (Double -> Double) -> L.Fold SimulationResult [(Double, Double)]
l2VsAlpha lowerBound upperBound bins density = M.elems <$> groupReplications ((,) <$> alphaMean <*> posteriorL2Mean lowerBound upperBound bins density )

nSimusVsAlpha :: L.Fold SimulationResult [(Double, Double)]
nSimusVsAlpha = (M.elems <$> groupReplications ((,) <$> numberSimusMean <*> alphaMean))

