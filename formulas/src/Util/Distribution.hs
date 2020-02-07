{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Rank2Types #-}

module Util.Distribution where

import Protolude
import Control.Monad.Random.Lazy
import Control.Monad.Primitive
import Control.Monad.ST
import qualified Data.Random as R
import Data.Word (Word64)
import qualified Data.Vector as V
import Statistics.Distribution hiding (mean)
import Statistics.Distribution.Normal
import Statistics.Distribution.Gamma
import qualified System.Random.MWC as MWC

data Uniform = Uniform {uniformLowerBound :: Double, uniformUpperBound :: Double} deriving (Eq, Show)
data Normal = Normal {normalMean :: Double, normalVar :: Double} deriving (Eq, Show)

uniformDensity :: (Double, Double) -> Double -> Double
uniformDensity (lowerBound, upperBound) x
  | x >= lowerBound && x <= upperBound = 1 / (upperBound - lowerBound)
  | otherwise = 0

uniformRandomSample :: (MonadRandom m) => (Double, Double) -> m Double
uniformRandomSample = getRandomR

normalDensity :: Double -> Double -> Double -> Double
normalDensity mean var x =
  exp(- ((x - mean) ** 2.0) / (2.0 * var)) / sqrt(2.0 * pi * var)

normalRandomSample :: forall m . (MonadRandom m) => Double -> Double -> m Double
normalRandomSample mean var =
  -- R.runRVar (R.normal mean (sqrt var)) (getRandom :: m Word64)
  do
    -- Initializing the generator of MWC at every function call is probably slow.
    -- TODO: Optimize by generalizing the use of MWC to the rest of the code such that
    -- the generator is only initialized once. Also benefit from a better random generator.
    seed <- pure <$> getRandom
    return $ runST $ do
      gen <- MWC.initialize . V.fromList $ seed
      genContinuous (normalDistr mean (sqrt var)) gen

normalCDF :: Double -> Double -> Double -> Double
normalCDF mean var = cumulative (normalDistr mean (sqrt var))

gammaRandomSample :: forall m . (MonadRandom m) => Double -> Double -> m Double
gammaRandomSample mean var
  -- The following two conditions are derived from the conditions k, theta > 0 on the parameter
  -- of the gamma distribution.
  | mean <= 0 = panic "Distribution.gammaRandomSample: mean parameter must be positive."
  | var <= 0 = panic "Distribution.gammaRandomSample: var parameter must be positive."
  | otherwise =
    let k = mean ** 2 / var
        theta = var / mean
    in do
      -- Initializing the generator of MWC at every function call is probably slow.
      -- TODO: Optimize by generalizing the use of MWC to the rest of the code such that
      -- the generator is only initialized once. Also benefit from a better random generator.
      seed <-  pure <$> getRandom
      return $ runST $ do
        gen <- MWC.initialize . V.fromList $ seed
        genContinuous (gammaDistr k theta) gen

