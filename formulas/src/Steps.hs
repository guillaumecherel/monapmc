{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Steps where

import Protolude 

import Control.Monad.Random.Lazy
import qualified Data.List as List
import qualified Data.Vector as V
import System.Random (StdGen, mkStdGen)

import Algorithm
import Figure
import Model
import Run
import Statistics
import qualified ABC.Lenormand2012 as Lenormand2012
import qualified ABC.SteadyState as SteadyState
import Util.Cache as Cache
import Util.CSV
import qualified Util.SteadyState as SteadyState 

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

lenormand2012Steps :: Rand StdGen (Cache [Run])
lenormand2012Steps = do
  g <- getSplit
  return $ cacheSteps $ fmap getRun $ evalRand steps g
  where steps :: Rand StdGen [(Int, Lenormand2012.S)]
        steps = zip [1..20] <$> Lenormand2012.scan p toyModel
        algo = Lenormand2012 5000 0.1 0.01
        p = Lenormand2012.P
          { Lenormand2012.n = getN algo
          , Lenormand2012.nAlpha = floor $ (getAlpha algo) * (fromIntegral $ getN algo)
          , Lenormand2012.pAccMin = getPAccMin algo
          , Lenormand2012.priorSample = toyPriorRandomSample
          , Lenormand2012.priorDensity = toyPrior
          , Lenormand2012.distanceToData = rootSquaredError 0 . V.head
          }
        getRun (i, s) = Run 
          { getAlgorithm = algo
          , getStep = i
          , getReplication = 1
          , getSample = Lenormand2012.thetas s }
        cacheSteps = cache' "output/formulas/5steps/toy/lenormand2012"
                     . pure
 
steadyStateSteps :: Rand StdGen (Cache [Run])
steadyStateSteps = do
  g <- getSplit
  return $ cacheSteps $ (fmap . fmap) getRun $ evalRandT steps g
  where steps :: RandT StdGen IO [(Int, SteadyState.S)]
        steps = zip needSteps <$> SteadyState.scanIndices needSteps ssr
        needSteps = [5000, 10000 .. 100000]        
        ssr = SteadyState.runner p model
        model (seed, xs) = return $ evalRand (toyModel xs) (mkStdGen seed)
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
        getRun (i, s) = Run 
          { getAlgorithm = algo
          , getStep = i
          , getReplication = 1
          , getSample = fmap (SteadyState.getTheta . SteadyState.getSimulation . SteadyState.getReady) (SteadyState.accepteds s) }
        cacheSteps = cache' "output/formulas/5steps/toy/lenormand2012"
                     . Cache.liftIO

rootSquaredError :: Double -> Double -> Double
rootSquaredError expected x = sqrt ((x - expected) ** 2)

histogramStep :: Run -> [(Double, Double)]
histogramStep = scaledHistogram (-10) 10 300 . V.toList . V.concat . V.toList . getSample

-- cacheHistogram :: FilePath -> Cache (V.Vector Double) -> Cache [(Double, Double)]
-- cacheHistogram path r = 
--   histogramStep <$> r
--   & cache path
--           (columns2 " ")
--           (bimap show identity . readHistogram path)

histogramsLenormand2012 :: Rand StdGen (Cache [[(Double, Double)]])
histogramsLenormand2012 = 
  lenormand2012Steps
  >>= return . (fmap . fmap) histogramStep
  >>= return . cache' "output/formulas/scaledHistogram/toy/lenormand2012" 

histogramsSteadyState :: Rand StdGen (Cache [[(Double, Double)]])
histogramsSteadyState = 
  steadyStateSteps
  >>= return . (fmap . fmap) histogramStep  
  >>= return . cache' "output/formulas/scaledHistogram/toy/steadyState"

histogramLenormand2012Step :: Int -> Rand StdGen (Cache ())
histogramLenormand2012Step i = do
  hs <- histogramsLenormand2012
  let cmh = List.lookup i . zip [1..] <$> hs 
  return $ sinkTxt
    ("output/formulas/scaledHistogram/toy/lenormand2012_" <> show i <> ".csv")
    (\mh -> case mh of
            Just h -> Right $ columns2 " " h 
            Nothing -> Left $ "No step " <> show i <> " for histogram Lenornand2012")
    cmh
        
histogramSteadyStateStep :: Int -> Rand StdGen (Cache ())
histogramSteadyStateStep i = do
  hs <- histogramsSteadyState
  let cmh = List.lookup i . zip [1..] <$> hs 
  return $ sinkTxt
    ("output/formulas/scaledHistogram/toy/steadyState_" <> show (i * 5000) <> ".csv")
    (\mh -> case mh of
            Just h -> Right $ columns2 " " h
            Nothing -> Left $ "No step " <> show i <> " for histogram SteadyState")
    cmh

figurePosteriorSteps :: Cache () 
figurePosteriorSteps =
  gnuplot "report/5steps.png" "report/5steps.gnuplot"
    [ ("formulas_lenormand2012_1", "output/formulas/scaledHistogram/toy/"
                               <>  "lenormand2012_1.csv")
    , ( "formulas_lenormand2012_2", "output/formulas/scaledHistogram/toy/"
                                 <> "lenormand2012_2.csv")
    , ( "formulas_lenormand2012_3", "output/formulas/scaledHistogram/toy/"
                                 <> "lenormand2012_3.csv")
    , ( "formulas_lenormand2012_4", "output/formulas/scaledHistogram/toy/"
                                 <> "lenormand2012_4.csv")
    , ( "formulas_lenormand2012_5", "output/formulas/scaledHistogram/toy/"
                                 <> "lenormand2012_5.csv")
    , ( "formulas_steadyState_1", "output/formulas/scaledHistogram/toy/"
                               <> "steadyState_5000.csv")
    , ( "formulas_steadyState_2", "output/formulas/scaledHistogram/toy/"
                               <> "steadyState_10000.csv")
    , ( "formulas_steadyState_3", "output/formulas/scaledHistogram/toy/"
                               <> "steadyState_15000.csv")
    , ( "formulas_steadyState_4", "output/formulas/scaledHistogram/toy/"
                               <> "steadyState_20000.csv")
    , ( "formulas_steadyState_5", "output/formulas/scaledHistogram/toy/"
                               <> "steadyState_25000.csv")
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



buildSteps :: Rand StdGen (Cache ())
buildSteps = liftA2 mappend (histogramLenormand2012Step 1) 
           $ liftA2 mappend (histogramLenormand2012Step 2)
           $ liftA2 mappend (histogramLenormand2012Step 3)
           $ liftA2 mappend (histogramLenormand2012Step 4)
           $ liftA2 mappend (histogramLenormand2012Step 5)
           $ liftA2 mappend (histogramSteadyStateStep 1)
           $ liftA2 mappend (histogramSteadyStateStep 2)
           $ liftA2 mappend (histogramSteadyStateStep 3)
           $ liftA2 mappend (histogramSteadyStateStep 4)
           $ liftA2 mappend (histogramSteadyStateStep 5)
                            (return $ figurePosteriorSteps)

