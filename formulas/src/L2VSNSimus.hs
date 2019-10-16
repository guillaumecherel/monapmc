{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}

module L2VSNSimus where

import Protolude 

import qualified Control.Foldl as Fold
import Control.Monad.Random.Lazy
import Data.Functor.Compose
import Data.Text (unlines, unpack)

import Algorithm
import Util.Figure
import Replications
import Run
import Statistics
import Util

data L2VSNSimus = L2VSNSimus
  { _l2Mean :: Double
  , _l2Std :: Double
  , _nSimusMean :: Double
  , _nSimusStd :: Double
  }

l2VSNSimus :: Algorithm -> [RunResult] -> L2VSNSimus
l2VSNSimus algo reps = Fold.fold f reps
  where f = L2VSNSimus <$> l2Mean <*> l2Std <*> nSimMean <*> nSimStd
        l2Mean = foldMeanWith Run.l2Toy
        l2Std = foldStdWith Run.l2Toy
        nSimMean =
          foldMeanWith (fromIntegral . nSimus algo . _stepCount)
        nSimStd =
          foldStdWith (fromIntegral . nSimus algo . _stepCount)

