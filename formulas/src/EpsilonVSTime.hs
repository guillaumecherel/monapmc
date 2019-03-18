{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module EpsilonVSTime where

import Protolude 

import Control.Monad.Random.Lazy
import Data.Cached
import Data.List (zipWith3)
import Data.Functor.Compose
import Data.Text (unpack)

import Algorithm
import Figure
import Replications
import Steps hiding (_epsilon, fig)
import qualified Steps (_epsilon)


data Point = Point {_time :: Double
                   ,_realTime :: Double
                   ,_epsilon :: Double}
  deriving (Eq, Show)

newtype EpsilonVSTime = EpsilonVSTime [Point]
  deriving (Eq, Show)

epsilonVSTime :: Algorithm -> StepsResult -> EpsilonVSTime
epsilonVSTime algo r = EpsilonVSTime $ zipWith3 Point cpuTime realTime
                    $ fmap (Steps._epsilon) $ _steps r
  where cpuTime = [1,2..]
        realTime = case algo of
                     Lenormand2012{} -> cpuTime
                     MonAPMC{getParallel=par} ->
                       fmap (\t -> fromIntegral $ ceiling $ t / fromIntegral par) cpuTime
                     _ -> repeat 0 -- undefined

epsilonVSTimeRep
  :: Replications
  -> Compose (Rand StdGen) Cached [EpsilonVSTime]
epsilonVSTimeRep reps =
  (fmap . fmap) (epsilonVSTime (Replications._algorithm reps))
    $ cachedRepSteps "output/formulas/cached" reps

fig :: Rand StdGen (Cached ())
fig = liftA2 (<>) (fmap mkFig cachedGPData)
                  (fmap mkFigReal cachedGPData)
  where
    parValues = [1,2,3,4]
    lenPath = "output/formulas/cached/epsilonVSTime/lenormand2012.csv"
    moaPath par = unpack "output/formulas/cached/epsilonVSTime/monAPMC_"
                    <> show par <> ".csv"
    repsLen = Replications
                { Replications._algorithm =
                    (Algorithm.Lenormand2012 5000 0.1 0.01)
                , Replications._stepMax = 100
                , Replications._nReplications = 5 }
    repsMoa par = Replications
                { Replications._algorithm =
                    (Algorithm.MonAPMC 5000 0.1 0.01 1 par)
                , Replications._stepMax = 100
                , Replications._nReplications = 5 }
    evtLen :: Rand StdGen (Cached [EpsilonVSTime])
    evtLen = getCompose $ epsilonVSTimeRep repsLen
    evtMoa :: Int -> Rand StdGen (Cached [EpsilonVSTime])
    evtMoa par = getCompose $ epsilonVSTimeRep (repsMoa par)
    evtAll :: Rand StdGen [Cached [EpsilonVSTime]]
    evtAll = liftA2 (:) evtLen (traverse evtMoa parValues)
    cachedGPData :: Rand StdGen [Cached [[(Double, Double, Double)]]]
    cachedGPData = (fmap . fmap . fmap . fmap) evtDouble evtAll
    evtDouble :: EpsilonVSTime -> [(Double, Double, Double)]
    evtDouble (EpsilonVSTime xys) =
      fmap ((,,) <$> _time <*> _realTime <*> _epsilon) xys
    figArgs :: [(Text, FilePath)]
    figArgs = [("lenormand2012", lenPath)]
                 <> fmap (\p -> ("monAPMC_" <> show p, moaPath p))
                         parValues
    mkFig :: [Cached [[(Double, Double, Double)]]]
          -> Cached ()
    mkFig = gnuplot3 "report/epsilon_vs_time.gnuplot"
                     "report/epsilon_vs_time.png"
                     (fmap (first unpack) figArgs)
    mkFigReal :: [Cached [[(Double, Double, Double)]]]
          -> Cached ()
    mkFigReal = gnuplot3 "report/epsilon_vs_realtime.gnuplot"
                         "report/epsilon_vs_realtime.png"
                     (fmap (first unpack) figArgs)
                   
buildEpsilonVSTime :: Rand StdGen (Cached ())
buildEpsilonVSTime = fig

