{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Statistics where

import Protolude
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Text.Parsec as P
import qualified Statistics.Sample.Histogram as S
import Util.Parser

-- Estimated posterior density given the posterior sample as list
-- [(weight_i, theta_i) | i <= N]
estPostDen
  :: Double -> Double -> Int -> [(Double, Double)] -> [(Double, Double)]
estPostDen lowerBound upperBound bins weightsXs =
  let width = (upperBound - lowerBound) / fromIntegral bins
      weightSum = sum $ fmap fst weightsXs
  in  Map.toAscList
        $ fmap (\s -> getSum s / (width * weightSum))
        $ Map.fromListWith (<>)
        $ fmap (\(w, x) -> (toBin lowerBound upperBound bins x, Sum w))
        $ filter (\(w, x) -> x >= lowerBound && x < upperBound) weightsXs

posteriorL2
  :: Double
  -> Double
  -> Int
  -> (Double -> Double)
  -> [(Double, Double)]
  -> Double
posteriorL2 lowerBound upperBound bins targetCDF weightsXs =
  let width       = (upperBound - lowerBound) / fromIntegral bins
      theoPostBin bin = targetCDF (bin + width) - targetCDF bin
      sumWeights = sum $ fmap fst weightsXs
      estPostBin = Map.toAscList
                   $ fmap (\s -> getSum s / sumWeights)
                   $ Map.fromListWith (<>)
                   $ fmap (\(w, x) ->
                            (toBin lowerBound upperBound bins x, Sum w))
                          weightsXs
  in  sqrt $ sum $ fmap (\(b, e) -> (e - theoPostBin b) ** 2) estPostBin
                              

toBin :: Double -> Double -> Int -> Double -> Double
toBin lowerBound upperBound bins x = lowerBound + width * fromIntegral (floor ((x - lowerBound) / width))
  where width = (upperBound - lowerBound) / fromIntegral bins

--------
-- Parsing
--------

readHistogram :: FilePath -> Text -> Either P.ParseError [(Double, Double)]
readHistogram = P.parse parserHistogram

parserHistogram :: (P.Stream s m Char) => P.ParsecT s u m [(Double, Double)]
parserHistogram = P.many $ P.try line
  where line = (,) <$> parserDouble
                   <*  P.many P.space
                   <*> parserDouble
                   <* P.optional P.endOfLine

