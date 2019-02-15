{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module L2VSNSimus where

import Protolude 

import qualified Control.Foldl as Fold
import Control.Monad.Random.Lazy
import Data.Cached as Cached
import Data.Functor.Compose

import Algorithm
import Figure
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

l2VSNSimus :: Run -> [RunResult] -> L2VSNSimus
l2VSNSimus r reps = Fold.fold f reps
  where f = L2VSNSimus <$> l2Mean <*> l2Std <*> nSimMean <*> nSimStd
        l2Mean = foldMeanWith l2Toy
        l2Std = foldStdWith l2Toy
        nSimMean = foldMeanWith (fromIntegral . nSimus r)
        nSimStd = foldStdWith (fromIntegral . nSimus r)

l2VSNSimus'
  :: Int -> Int -> Algorithm
  -> Compose (Rand StdGen) Cached L2VSNSimus
l2VSNSimus' nRep stepMax algo =
  l2VSNSimus (Run algo stepMax) <$> cachedRepRuns "output/formulas"
    (Replications algo stepMax nRep)

fig :: Compose (Rand StdGen) Cached ()
fig = 
  let nReplications = 10 -- 10
      alphas = [1%10, 2%10 .. 9%10] -- [0.1,0.2..0.9]
      nAlpha = 500
      pAccMins = [0.01, 0.05, 0.1, 0.2]
      stepMax = 100
      lenPath = "output/formulas/l2VSNSimus/lenormand2012.csv"
      moaPath = "output/formulas/l2VSNSimus/monAPMC.csv"
      len pAccMin alpha = Lenormand2012
                            { getN = floor (nAlpha / alpha)
                            , getAlpha=fromRational alpha
                            , getPAccMin=pAccMin}
      moa pAccMin alpha = MonAPMC
                            { getN = floor (nAlpha / alpha)
                            , getAlpha = fromRational alpha
                            , getPAccMin = pAccMin
                            , getStepSize = 1
                            , getParallel = 2}
      algos al = fmap (\pAccMin -> fmap (al pAccMin) alphas) pAccMins
      gnuplotData al =
        fmap (gnuplotData4 _nSimusMean _l2Mean _nSimusStd _l2Std)
        $ (traverse . traverse) (l2VSNSimus' nReplications stepMax)
        $ algos al
      gnuplotInputFile path al =
        liftCR (gnuplotDataSink path)
        $ gnuplotData al
      fig' :: Cached ()
      fig' = gnuplot "report/L2_vs_nsimus.png" "report/L2_vs_nsimus.gnuplot"
                        [("lenormand2012", lenPath),
                         ("monAPMC", moaPath)]

  in foldr (liftCR2 (<>))  (Compose (pure fig'))
                          [ gnuplotInputFile lenPath len
                          , gnuplotInputFile moaPath moa ]

buildL2VSNSimus :: Rand StdGen (Cached ())
buildL2VSNSimus = getCompose fig

