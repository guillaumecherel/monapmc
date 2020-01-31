{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}

module Model where

import Protolude
import Control.Monad.Random.Lazy
import qualified Data.Random as R
import qualified Data.Vector as V
import           Util.Duration (Duration, fromSeconds)
-- import qualified Text.Parsec as P

import Util.Distribution as Distribution

data Model
  = Toy
  | ToyTimeBias Double Double deriving (Eq, Show, Read)

model :: (MonadRandom m) => Model -> V.Vector Double -> m (Duration, V.Vector Double)
model Toy = (fmap . fmap) (1,) toyModel
model (ToyTimeBias mean var) = undefined

priorRandomSample :: (MonadRandom m) => Model -> m (V.Vector Double)
priorRandomSample Toy = toyPriorRandomSample
priorRandomSample (ToyTimeBias mean var) = undefined

prior :: Model -> V.Vector Double -> Double
prior Toy = toyPrior
prior (ToyTimeBias mean var) = undefined

--------
-- Toy Model
--------

toyModelVar1 :: Double
toyModelVar1 = 1 / 100
toyModelVar2 :: Double
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

toyPosteriorCDF :: Double -> Double -> Double
toyPosteriorCDF theta x =
  0.5 * normalCDF theta toyModelVar1 x
    + 0.5 * normalCDF theta toyModelVar2 x

toyPosteriorRegularSample :: Double -> Double -> Double -> Int -> [(Double, Double)]
toyPosteriorRegularSample theta lowerBound upperBound samples =
  let every = (upperBound - lowerBound) / fromIntegral samples
  in [(x, toyPosterior theta x) | x <- [lowerBound, lowerBound + every .. upperBound]]

--------
-- Toy Model with time bias
--------

-- toyModelTimeBias ::  forall m . (MonadRandom m) => V.Vector Double -> m (Double, (V.Vector Double))
-- toyModelTimeBias = undefined



--------
-- Parsing
--------

-- readModel :: (MonadRandom m) => Text -> Either P.ParseError (V.Vector Double -> m (V.Vector Double))
-- readModel x = do
--   m <- P.parse parserModel "" x
--   case m of
--     Toy -> return $ toyModel
-- 
-- parserModel :: (P.Stream s m Char) => P.ParsecT s u m Model
-- parserModel = P.string "toyModel" *> pure Toy

