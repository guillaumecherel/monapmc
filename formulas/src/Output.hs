{-# LANGUAGE OverloadedStrings #-}

module Output where
  
import Prelude (undefined, ($))

import Data.Maybe (Maybe(..))
import Data.Functor (fmap)
import Data.List (take, length)
import Data.Ord ((>), (<))
import Data.Monoid ((<>))
import qualified Data.Vector as V
import Data.Text (Text, unlines, unpack)
import TextShow (TextShow, showt, toText)
import TextShow.Data.Floating (showbFFloat)
import System.FilePath (FilePath)

import Formulas

pprintAlgorithm :: Algorithm -> Text
pprintAlgorithm Lenormand2012
  { getN = n
  , getAlpha = alpha
  , getPAccMin = pAccMin
  } =
  "Lenormand2012 n=" <> (showt n)
            <> " alpha=" <> (showt alpha)
            <> " pAccMin=" <> (showt pAccMin)
pprintAlgorithm Beaumont2009
  { getN=n
  , getEpsilonTo=epsilonTo
  , getEpsilonFrom=epsilonFrom
  } =
  "Beaumont2009 n=" <> (showt n) <> " epsilonFrom=" <> (showt epsilonFrom) <> " epsilonTo=" <> (showt epsilonTo)
pprintAlgorithm SteadyState
  { getN=n
  , getAlpha=alpha
  , getPAccMin=pAccMin
  , getParallel=parallel
  } =
  "SteadyState n=" <> (showt n) <> " alpha=" <> (showt alpha) <> " pAccMin=" <> (showt pAccMin) <> " parallel=" <> (showt parallel)

pprint :: SimulationResult -> Text
pprint s = pprintAlgorithm (getAlgorithm s)
        <> " step=" <> (showt $ getStep s)
        <> " replication=" <> (showt $ getReplication s)
        <> " sample=" <> (showt $ take 3 $ V.toList $ fmap V.head $ getSample s)
        <> if (length (getSample s) > 3) then "..." else ""

column :: TextShow a => [a] -> Text
column xs = unlines $ fmap showt xs

columns2 :: (TextShow a, TextShow b) => Text -> [(a, b)] -> Text
columns2 _ [] = ""
columns2 sep ((a,b):xs) = showt a <> sep <> showt b <> "\n" <> columns2 sep xs

simulationResultFileName :: SimulationResult -> FilePath
simulationResultFileName s = unpack $
     algorithmPart (getAlgorithm s) <> "_"
  <> showt (getStep s) <> "_"
  <> showt (getReplication s) <> ".csv"
  where algorithmPart (Lenormand2012 {getN=n, getAlpha=alpha, getPAccMin=pAccMin}) =
          "lenormand2012_" <> showt n <> "_" <> show2dec alpha <> "_" <> show2dec pAccMin
        algorithmPart (Beaumont2009 {getN=n, getEpsilonFrom=eFrom, getEpsilonTo=eTo}) =
          "beaumont2009_" <> showt n <> "_" <> show2dec eFrom <> "_" <> show2dec eTo
        algorithmPart (SteadyState {getN=n, getAlpha=alpha, getPAccMin=pAccMin, getParallel=par}) =
          "steadyState_" <> showt n <> "_" <> show2dec alpha <> "_" <> show2dec pAccMin <> "_" <> showt par

show2dec x = toText (showbFFloat (Just 2) x)

-- save :: FilePath -> SimulationResult -> IO ()
-- save dir res = do

