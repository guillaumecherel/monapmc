{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Distribution where

import Protolude
import Control.Monad.Random.Lazy
import qualified Data.Random as R
import Data.Word (Word64)
import qualified Text.Parsec as P
import Util.Parser

data Uniform = Uniform {lowerBound :: Double, upperBound :: Double} deriving (Eq, Show)
data Normal = Normal {mean :: Double, var :: Double} deriving (Eq, Show)

class Sampleable d a | d -> a where
  sample :: (MonadRandom m) => d -> m a

instance Sampleable Uniform Double where
  sample (Uniform {lowerBound = l, upperBound = u}) = uniformRandomSample (l, u)

instance Sampleable Normal Double where
  sample (Normal {mean = m, var = v}) = normalRandomSample m v

class Continuous d a | d -> a where
  density :: d -> a -> Double

instance Continuous Uniform Double where
  density (Uniform l u) = uniformDensity (u, l)

instance Continuous Normal Double where
  density (Normal m v) = normalDensity m v

class Discrete d a | d -> a where
  proba :: d -> a -> Double

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
  R.runRVar (R.normal mean (sqrt var)) (getRandom :: m Word64)


--------
-- Parsing
--------

readDensity :: String -> Either P.ParseError (Double -> Double)
-- readDensity "toyPosterior" = Right $ toyPosterior 0
readDensity x = P.parse parserDensity "" x

readSamplingDouble :: (MonadRandom m) => String -> Either P.ParseError (m Double)
readSamplingDouble x = P.parse parserSamplingFunction "" x

parserDensity :: (P.Stream s m Char) => P.ParsecT s u m (Double -> Double)
parserDensity =
      P.try (parserUniform >>= \(Uniform u l) -> return (uniformDensity (u, l)))
  <|> (parserNormal >>= \(Normal m v) -> return (normalDensity m v))

parserSamplingFunction :: (P.Stream s m Char, MonadRandom n) => P.ParsecT s u m (n Double)
parserSamplingFunction =
      P.try (parserUniform >>= \(Uniform u l) -> return (uniformRandomSample (u, l)))
   <|> (parserNormal >>= \(Normal m v) -> return (normalRandomSample m v))

parserUniform :: (P.Stream s m Char) => P.ParsecT s u m Uniform
parserUniform = do
  P.string "uniform" *> P.spaces
  lowerBound <- parserDouble <* P.spaces
  upperBound <- parserDouble <* P.spaces
  return $ Uniform lowerBound upperBound

parserNormal :: (P.Stream s m Char) => P.ParsecT s u m Normal
parserNormal = do
  P.string "normal" *> P.spaces
  mean <- parserDouble <* P.spaces
  var <- parserDouble <* P.spaces
  return $ Normal mean var

