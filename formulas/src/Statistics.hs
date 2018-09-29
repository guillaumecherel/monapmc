{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Statistics where

import Protolude
import qualified Data.Vector as V
import qualified Text.Parsec as P
import qualified Statistics.Sample.Histogram as S
import Util.Parser

histogram :: Double -> Double -> Int -> [Double] -> [(Double, Double)]
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

posteriorL2 :: Double -> Double -> Int -> (Double -> Double) -> [Double] -> Double
posteriorL2 lowerBound upperBound bins targetDensity xs =
  let scaledHist = scaledHistogram lowerBound upperBound bins xs
      binWidth = (upperBound - lowerBound) / fromIntegral bins
      density = [targetDensity (x + (binWidth / 2.0)) | (x,_) <- scaledHist]
      scaledHistHeights = map snd scaledHist
  in sqrt $ getSum $ foldMap (\(d,h) -> Sum $ (d - h) ** 2) (zip density scaledHistHeights)




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

