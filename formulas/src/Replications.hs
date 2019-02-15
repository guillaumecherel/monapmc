{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Replications where

import Protolude 

import Data.Functor.Compose
import Control.Monad.Random.Lazy
import Data.Cached 
import Data.Text (unpack)
import System.FilePath ((</>))

import Algorithm
import Run (RunResult, runResult, run)
import Steps (StepsResult, stepsResult, steps)

data Replications = Replications
  { _algorithm :: Algorithm
  , _stepMax :: Int
  , _nReplications :: Int }
  deriving (Eq, Show, Read)

repRuns :: Replications -> Rand StdGen (IO [RunResult])
repRuns r =
   fmap sequence
   $ sequence
   $ replicate (_nReplications r)
   $ runResult (run (_stepMax r) (_algorithm r))

repSteps :: Replications -> Rand StdGen (IO [StepsResult])
repSteps r =
   fmap sequence
   $ sequence
   $ replicate (_nReplications r)
   $ stepsResult (steps (_stepMax r) (_algorithm r))

cachedRepRuns
  :: FilePath -> Replications -> Compose (Rand StdGen) Cached ([RunResult])
cachedRepRuns rootDir r =
  Compose $ fmap (cache' (rootDir </> repRunsFilename r) . fromIO mempty)
          $ repRuns r

cachedRepSteps 
  :: FilePath -> Replications -> Compose (Rand StdGen) Cached ([StepsResult])
cachedRepSteps rootDir r =
  Compose $ fmap (cache' (rootDir </> repStepsFilename r) . fromIO mempty)
          $ repSteps r

repRunsFilename :: Replications -> FilePath
repRunsFilename Replications{_algorithm=algo, _stepMax=stepMax, _nReplications=nRep} =
  ( unpack $ "replications_runs_" <> show nRep <> "_" <> show stepMax)
    </> algoFilename algo

repStepsFilename :: Replications -> FilePath
repStepsFilename Replications{_algorithm=algo, _stepMax=stepMax, _nReplications=nRep} =
  ( unpack $ "replications_steps_" <> show nRep <> "_" <> show stepMax)
    </> algoFilename algo
