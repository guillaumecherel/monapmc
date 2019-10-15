{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Algorithm where

import Protolude

import Data.Text (unpack)
import Formatting

data Algorithm =
  APMC
    { getN :: Int
    , getNAlpha :: Int
    , getPAccMin :: Double
    }
  | MonAPMC
    { getN :: Int
    , getNAlpha :: Int
    , getPAccMin :: Double
    , getStepSize :: Int
    , getParallel :: Int
    , getStopSampleSizeFactor :: Int
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
  deriving (Show, Read, Eq, Ord)

algoFilename :: Algorithm -> FilePath
algoFilename APMC{getN=n, getNAlpha=nAlpha, getPAccMin=pAccMin} =
   unpack $ "apmc_"
             <> show n <> "_"
             <> sformat (fixed 2) nAlpha <> "_"
             <> sformat (fixed 2) pAccMin
algoFilename MonAPMC{getN=n, getNAlpha=nAlpha, getPAccMin=pAccMin, getStepSize=stepSize, getParallel=parallel, getStopSampleSizeFactor=sf} =
   unpack $ "monAPMC_"
             <> show n <> "_"
             <> show nAlpha <> "_"
             <> sformat (fixed 2) pAccMin <> "_"
             <> show stepSize <> "_"
             <> show parallel <> "_"
             <> show sf
algoFilename SteadyState{getN=n, getAlpha=alpha, getPAccMin=pAccMin, getParallel=par} =
   unpack $ "steadyState_"
             <> show n <> "_"
             <> sformat (fixed 2) alpha <> "_"
             <> sformat (fixed 2) pAccMin <> "_"
             <> show par
algoFilename Beaumont2009{getN=n, getEpsilonFrom=ef, getEpsilonTo=et} =
   unpack $ "beaumont2009"
             <> show n <> "_"
             <> sformat (fixed 2) ef <> "_"
             <> sformat (fixed 2) et


cpuTime :: Int -> Double
cpuTime step = fromIntegral step

realTime :: Algorithm -> Int -> Double
realTime algo step = case algo of
  APMC{} -> cpuTime step
  MonAPMC{getStepSize=stepSize, getParallel=par} ->
   fromIntegral $ stepSize * (ceiling $ cpuTime step / fromIntegral (par * stepSize))
  _ -> panic "Algorithm.realTime Not implemented."
