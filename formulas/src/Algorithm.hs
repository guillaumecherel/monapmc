{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Algorithm where

import Protolude

import Data.Text (unpack)
import Formatting

data Algorithm =
  Lenormand2012
    { getN :: Int
    , getAlpha :: Double
    , getPAccMin :: Double
    }
  | MonAPMC
    { getN :: Int
    , getAlpha :: Double
    , getPAccMin :: Double
    , getStepSize :: Int
    , getParallel :: Int
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
algoFilename Lenormand2012{getN=n, getAlpha=alpha, getPAccMin=pAccMin} =
   unpack $ "lenormand2012_"
             <> show n <> "_"
             <> sformat (fixed 2) alpha <> "_"
             <> sformat (fixed 2) pAccMin
algoFilename MonAPMC{getN=n, getAlpha=alpha, getPAccMin=pAccMin, getStepSize=stepSize, getParallel=parallel} =
   unpack $ "monAPMC_"
             <> show n <> "_"
             <> sformat (fixed 2) alpha <> "_"
             <> sformat (fixed 2) pAccMin <> "_"
             <> show stepSize <> "_"
             <> show parallel
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

