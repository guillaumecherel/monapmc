{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified Text.Parsec as P
import Util.Parser

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
gammaRandomSample mode var =
  let std = sqrt var
      k = std / (std - mode)
      theta = mode / (k - 1)
  in do
    -- Initializing the generator of MWC at every function call is probably slow.
    -- TODO: Optimize by generalizing the use of MWC to the rest of the code such that
    -- the generator is only initialized once. Also benefit from a better random generator.
    seed <-  pure <$> getRandom
    return $ runST $ do
      gen <- MWC.initialize . V.fromList $ seed
      genContinuous (gammaDistr k theta) gen

--------
-- Parsing
--------

---- TO BE REMOVED
-- class Sampleable d a | d -> a where
--   sample :: (MonadRandom m) => d -> m a
-- 
-- instance Sampleable Uniform Double where
--   sample (Uniform {uniformLowerBound = l, uniformUpperBound = u}) = uniformRandomSample (l, u)
-- 
-- instance Sampleable Normal Double where
--   sample (Normal {normalMean = m, normalVar = v}) = normalRandomSample m v
-- 
-- class Continuous d a | d -> a where
--   density :: d -> a -> Double
-- 
-- instance Continuous Uniform Double where
--   density (Uniform l u) = uniformDensity (u, l)
-- 
-- instance Continuous Normal Double where
--   density (Normal m v) = normalDensity m v
-- 
-- class Discrete d a | d -> a where
--   proba :: d -> a -> Double
-- 
-- readDensity :: String -> Either P.ParseError (Double -> Double)
-- -- readDensity "toyPosterior" = Right $ toyPosterior 0
-- readDensity x = P.parse parserDensity "" x
-- 
-- readSamplingDouble :: (MonadRandom m) => String -> Either P.ParseError (m Double)
-- readSamplingDouble x = P.parse parserSamplingFunction "" x
-- 
-- parserDensity :: (P.Stream s m Char) => P.ParsecT s u m (Double -> Double)
-- parserDensity =
--       P.try (parserUniform >>= \(Uniform u l) -> return (uniformDensity (u, l)))
--   <|> (parserNormal >>= \(Normal m v) -> return (normalDensity m v))
-- 
-- parserSamplingFunction :: (P.Stream s m Char, MonadRandom n) => P.ParsecT s u m (n Double)
-- parserSamplingFunction =
--       P.try (parserUniform >>= \(Uniform u l) -> return (uniformRandomSample (u, l)))
--    <|> (parserNormal >>= \(Normal m v) -> return (normalRandomSample m v))
-- 
-- parserUniform :: (P.Stream s m Char) => P.ParsecT s u m Uniform
-- parserUniform = do
--   P.string "uniform" *> P.spaces
--   lowerBound <- parserDouble <* P.spaces
--   upperBound <- parserDouble <* P.spaces
--   return $ Uniform lowerBound upperBound
-- 
-- parserNormal :: (P.Stream s m Char) => P.ParsecT s u m Normal
-- parserNormal = do
--   P.string "normal" *> P.spaces
--   mean <- parserDouble <* P.spaces
--   var <- parserDouble <* P.spaces
--   return $ Normal mean var
-- 
