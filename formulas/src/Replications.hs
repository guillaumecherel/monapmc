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
import Figure
import Model
import Run
import Statistics

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
  algo@Lenormand2012{getN=n, getAlpha=alpha, getPAccMin=pAccMin} =
   unpack $ "output/formulas/replications/lenormand2012_"
             <> show nRep <> "_"
             <> show stepMax <> "_"
             <> show n <> "_"
             <> sformat (fixed 2) alpha <> "_"
             <> sformat (fixed 2) pAccMin
cachePath nRep stepMax
  algo@SteadyState{getN=n, getAlpha=alpha, getPAccMin=pAccMin, getParallel=par} =
   unpack $ "output/formulas/replications/steadyState_"
             <> show nRep <> "_"
             <> show stepMax <> "_"
             <> show n <> "_"
             <> sformat (fixed 2) alpha <> "_"
             <> sformat (fixed 2) pAccMin <> "_"
             <> show par
cachePath nRep stepMax
  algo@Beaumont2009{getN=n, getEpsilonFrom=ef, getEpsilonTo=et} = undefined