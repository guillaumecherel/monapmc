{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Steps where

import Protolude 

import qualified Control.Foldl as L
import Control.Monad.Fail (fail)
import Control.Monad.Random (Rand, RandT, MonadRandom, evalRand, evalRandT)
import qualified Data.Map as Map
import Data.String (String)
import Data.Text (Text, pack, unpack, replace)
import Data.Text.IO (writeFile)
import qualified Data.Vector as V
import System.Random (StdGen, mkStdGen)
-- import Numeric
import System.FilePath (FilePath, (</>))
import System.FilePath.Glob (glob)
import System.Process (callProcess)
import Text.Printf (printf)

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import Algorithm
import Figure
import Model
import Simulation
import Statistics
import qualified ABC.Lenormand2012 as Lenormand2012
import qualified ABC.SteadyState as SteadyState
import Util.Cache
import Util.CSV
import qualified Util.SteadyState as SteadyState (start, step, scanIndices) 

algoLenormand2012 :: Algorithm
algoLenormand2012 = Lenormand2012 { getN = 5000
                                  , getAlpha = 0.1
                                  , getPAccMin = 0.01}

algoSteadyState :: Algorithm
algoSteadyState = SteadyState { getN = 5000
                              , getAlpha = 0.1
                              , getPAccMin = 0.01
                              , getParallel = 1
                              }

lenormand2012Steps :: [Cache SimulationResult]
lenormand2012Steps = 
  let steps = zip [1..] $ take 20 $ evalRand (Lenormand2012.scan p toyModel :: Rand StdGen [Lenormand2012.S]) (mkStdGen seed)
      seed = 41
      algo = Lenormand2012 5000 0.1 0.01
      p = Lenormand2012.P
        { Lenormand2012.n = getN algo
        , Lenormand2012.nAlpha = floor $ (getAlpha algo) * (fromIntegral $ getN algo)
        , Lenormand2012.pAccMin = getPAccMin algo
        , Lenormand2012.priorSample = toyPriorRandomSample
        , Lenormand2012.priorDensity = toyPrior
        , Lenormand2012.distanceToData = rootSquaredError 0 . V.head
        }
      getSimulationResult (i, s) = SimulationResult 
        { getAlgorithm = algo
        , getStep = i
        , getReplication = 1
        , getSample = Lenormand2012.thetas s }
  in fmap (cacheSimulationResult "5steps" . getSimulationResult) steps
 
steadyStateSteps :: IO [Cache SimulationResult]
steadyStateSteps = 
  (fmap . fmap) (cacheSimulationResult "5steps" . getSimulationResult) enumSteps
  where enumSteps = zip needSteps <$> steps
        needSteps = [5000, 10000 .. 100000]
        steps = flip evalRandT (mkStdGen startSeed) scan
        scan = SteadyState.scanIndices needSteps ssr
        ssr = SteadyState.runner p model
        model (seed, xs) = return $ evalRand (toyModel xs) (mkStdGen seed)
        startSeed = 41
        algo = SteadyState 5000 0.1 0.01 1
        p = SteadyState.P
          { SteadyState.n = getN algo
          , SteadyState.nAlpha = floor $ (getAlpha algo) * (fromIntegral $ getN algo)
          , SteadyState.pAccMin = getPAccMin algo
          , SteadyState.parallel = getParallel algo
          , SteadyState.priorSample = toyPriorRandomSample
          , SteadyState.priorDensity = toyPrior
          , SteadyState.distanceToData = rootSquaredError 0 . V.head
          }
        getSimulationResult (i, s) = SimulationResult 
          { getAlgorithm = algo
          , getStep = i
          , getReplication = 1
          , getSample = fmap (SteadyState.getTheta . SteadyState.getSimulation . SteadyState.getReady) (SteadyState.accepteds s) }
        

rootSquaredError expected x = sqrt ((x - expected) ** 2)

histogramStep :: SimulationResult -> [(Double, Double)]
histogramStep s = scaledHistogram (-10) 10 300 . V.toList . fmap V.head . getSample $ s

cachedHistogram :: FilePath -> Cache SimulationResult -> Cache [(Double, Double)]
cachedHistogram dir s = 
  cPure histogramStep `cAp` s
  & cacheAsTxt filename
            (columns2 " ")
            (bimap show identity . readHistogram filename)
  where filename = dir </> takeFileName (cachePath s)

histogramsLenormand2012 :: [Cache [(Double, Double)]]
histogramsLenormand2012 = 
  fmap (cachedHistogram "output/formulas/scaledHistogram/toy/")
       lenormand2012Steps

histogramsSteadyState :: IO [Cache [(Double, Double)]]
histogramsSteadyState = 
  (fmap . fmap) (cachedHistogram "output/formulas/scaledHistogram/toy/") 
                steadyStateSteps

figurePosteriorSteps :: Sink 
figurePosteriorSteps =
  gnuplot "report/5steps.png" "report/5steps.gnuplot"
    [ ("formulas_lenormand2012_1", "output/formulas/scaledHistogram/toy/"
                               <>  "lenormand2012_5000_0.10_0.01_1_1.csv")
    , ( "formulas_lenormand2012_2", "output/formulas/scaledHistogram/toy/"
                                 <> "lenormand2012_5000_0.10_0.01_2_1.csv")
    , ( "formulas_lenormand2012_3", "output/formulas/scaledHistogram/toy/"
                                 <> "lenormand2012_5000_0.10_0.01_3_1.csv")
    , ( "formulas_lenormand2012_4", "output/formulas/scaledHistogram/toy/"
                                 <> "lenormand2012_5000_0.10_0.01_4_1.csv")
    , ( "formulas_lenormand2012_5", "output/formulas/scaledHistogram/toy/"
                                 <> "lenormand2012_5000_0.10_0.01_5_1.csv")
    , ( "formulas_steadyState_1", "output/formulas/scaledHistogram/toy/"
                               <> "steadyState_5000_0.10_0.01_1_5000_1.csv")
    , ( "formulas_steadyState_2", "output/formulas/scaledHistogram/toy/"
                               <> "steadyState_5000_0.10_0.01_1_10000_1.csv")
    , ( "formulas_steadyState_3", "output/formulas/scaledHistogram/toy/"
                               <> "steadyState_5000_0.10_0.01_1_15000_1.csv")
    , ( "formulas_steadyState_4", "output/formulas/scaledHistogram/toy/"
                               <> "steadyState_5000_0.10_0.01_1_20000_1.csv")
    , ( "formulas_steadyState_5", "output/formulas/scaledHistogram/toy/"
                               <> "steadyState_5000_0.10_0.01_1_25000_1.csv")
    , ( "easyABC_lenormand2012_1", "output/easyABC/scaledHistogram/toy/"
                               <> "lenormand2012_5000_0.10_0.01_0_1.csv")
    , ( "easyABC_lenormand2012_2", "output/easyABC/scaledHistogram/toy/"
                                <> "lenormand2012_5000_0.10_0.01_1_1.csv")
    , ( "easyABC_lenormand2012_3", "output/easyABC/scaledHistogram/toy/"
                                <> "lenormand2012_5000_0.10_0.01_2_1.csv")
    , ( "easyABC_lenormand2012_4", "output/easyABC/scaledHistogram/toy/"
                                <> "lenormand2012_5000_0.10_0.01_3_1.csv")
    , ( "easyABC_lenormand2012_5", "output/easyABC/scaledHistogram/toy/"
                                <> "lenormand2012_5000_0.10_0.01_4_1.csv")
    , ( "easyABC_beaumont2009_1", "output/easyABC/scaledHistogram/toy/"
                               <> "beaumont2009_5000_2.00_0.01_0_1.csv")
    , ( "easyABC_beaumont2009_2", "output/easyABC/scaledHistogram/toy/"
                               <> "beaumont2009_5000_2.00_0.01_1_1.csv")
    , ( "easyABC_beaumont2009_3", "output/easyABC/scaledHistogram/toy/"
                               <> "beaumont2009_5000_2.00_0.01_2_1.csv")
    , ( "easyABC_beaumont2009_4", "output/easyABC/scaledHistogram/toy/"
                               <> "beaumont2009_5000_2.00_0.01_3_1.csv")
    , ( "easyABC_beaumont2009_5", "output/easyABC/scaledHistogram/toy/"
                               <> "beaumont2009_5000_2.00_0.01_4_1.csv")
    ]



buildSteps :: Rules ()
buildSteps = foldMap buildCache histogramsLenormand2012
          <> foldMap buildCache lenormand2012Steps
          <> join (liftIO $ (fmap . foldMap) buildCache steadyStateSteps)
          <> join (liftIO $ (fmap . foldMap) buildCache histogramsSteadyState)
          <> buildSink figurePosteriorSteps

