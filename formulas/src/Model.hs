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
import Statistics (posteriorL2)

data Model
  = Toy
  | ToyTimeVar Double Double
  -- ToyTimeVar mean_run_time variance_run_time
  | ToyTimeBias Double Double Double Double
  -- ToyTimeBias bias_factor mean_run_time variance_run_time
  deriving (Eq, Show, Read)

model :: (MonadRandom m) => Model -> V.Vector Double -> m (Duration, V.Vector Double)
model Toy x = (fromSeconds 1,) <$> toyModel x
model (ToyTimeVar mean var) x =
  (,) <$> (fromSeconds <$> gammaRandomSample mean var) <*> toyModel x
model (ToyTimeBias biasFactor biasThreshold mean var) x =
  (,) <$> (fromSeconds <$> biasedRandomSample (V.head x)) -- <*> toyModel x
  <*> (V.singleton <$> uniformRandomSample (V.head x - 1, V.head x + 1))
  -- (,) <$> (fromSeconds <$> gammaRandomSample (max 1 (power * (V.head x + 1))) var) <*> toyModel x
  where
    biasedRandomSample x' = if x' < biasThreshold
      then gammaRandomSample mean var
      else (biasFactor *) <$> gammaRandomSample mean var

priorRandomSample :: (MonadRandom m) => Model -> m (V.Vector Double)
priorRandomSample Toy = toyPriorRandomSample
priorRandomSample (ToyTimeVar _ _) = toyPriorRandomSample
priorRandomSample (ToyTimeBias _ _ _ _) = toyPriorRandomSample

prior :: Model -> V.Vector Double -> Double
prior Toy = toyPrior
prior (ToyTimeVar _ _) = toyPrior
prior (ToyTimeBias _ _ _ _) = toyPrior

posterior :: Model -> V.Vector Double -> Double
posterior Toy = toyPosteriorCDF 0 . V.head
posterior (ToyTimeVar _ _) = toyPosteriorCDF 0 . V.head
posterior (ToyTimeBias _ _ _ _) = uniformCDF (-1, 1) . V.head

l2 :: Model -> V.Vector (Double, V.Vector Double) -> Double
l2 m@(Toy) sample = posteriorL2 (-10) 10 300 (posterior m . V.singleton)
                 (V.toList $ second V.head <$> sample)
l2 m@(ToyTimeVar _ _) sample = posteriorL2 (-10) 10 300 (posterior m . V.singleton)
                 (V.toList $ second V.head <$> sample)
l2 m@(ToyTimeBias _ _ _ _) sample = posteriorL2 (-10) 10 300 (posterior m . V.singleton)
                 (V.toList $ second V.head <$> sample)



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

