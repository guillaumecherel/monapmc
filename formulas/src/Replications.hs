{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Replications where

import Protolude 

import Data.Functor.Compose
import Control.Monad.Random.Lazy
import Data.Cached 
import Data.Text (unpack)
import Formatting

import Algorithm
import Run

newtype Replications = Replications {_replications :: [Run]}
  deriving (Eq, Show, Read)

replications :: Int -> Int -> Algorithm -> Rand StdGen (IO Replications)
replications nRep stepMax algo =
  (fmap . fmap) Replications
   $ fmap sequence
   $ sequence
   $ replicate nRep
   $ run stepMax algo

cachedReplications
  :: Int -> Int -> Algorithm -> Compose (Rand StdGen) Cached Replications
cachedReplications nRep stepMax algo =
  Compose $ fmap (cache' (cachePath nRep stepMax algo) . fromIO mempty)
          $ replications nRep stepMax algo

cachePath :: Int -> Int -> Algorithm -> FilePath
cachePath nRep stepMax
  Lenormand2012{getN=n, getAlpha=alpha, getPAccMin=pAccMin} =
   unpack $ "output/formulas/replications/lenormand2012_"
             <> show nRep <> "_"
             <> show stepMax <> "_"
             <> show n <> "_"
             <> sformat (fixed 2) alpha <> "_"
             <> sformat (fixed 2) pAccMin
-- cachePath nRep stepMax
--   MonAPMCSeq{getN=n, getAlpha=alpha, getPAccMin=pAccMin} =
--    unpack $ "output/formulas/replications/monAPMCSeq_"
--              <> show nRep <> "_"
--              <> show stepMax <> "_"
--              <> show n <> "_"
--              <> sformat (fixed 2) alpha <> "_"
--              <> sformat (fixed 2) pAccMin
cachePath nRep stepMax
  MonAPMC{getN=n, getAlpha=alpha, getPAccMin=pAccMin, getStepSize=stepSize, getParallel=parallel} =
   unpack $ "output/formulas/replications/monAPMC_"
             <> show nRep <> "_"
             <> show stepMax <> "_"
             <> show n <> "_"
             <> sformat (fixed 2) alpha <> "_"
             <> sformat (fixed 2) pAccMin <> "_"
             <> show stepSize <> "_"
             <> show parallel
cachePath nRep stepMax
  SteadyState{getN=n, getAlpha=alpha, getPAccMin=pAccMin, getParallel=par} =
   unpack $ "output/formulas/replications/steadyState_"
             <> show nRep <> "_"
             <> show stepMax <> "_"
             <> show n <> "_"
             <> sformat (fixed 2) alpha <> "_"
             <> sformat (fixed 2) pAccMin <> "_"
             <> show par
cachePath nRep stepMax
  Beaumont2009{getN=n, getEpsilonFrom=ef, getEpsilonTo=et} = 
   unpack $ "output/formulas/replications/beaumont2009"
             <> show nRep <> "_"
             <> show stepMax <> "_"
             <> show n <> "_"
             <> sformat (fixed 2) ef <> "_"
             <> sformat (fixed 2) et
