{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Steps where

import Protolude 

import Data.Cached as Cached
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

lenormand2012Steps :: Rand StdGen (Cached [Run])
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
          , Lenormand2012.distanceToData = absoluteError 0 . V.head
          }
        getRun (i, s) = Run 
          { getAlgorithm = algo
          , getStep = i
          , getReplication = 1
          , getSample = V.zip (Lenormand2012.weights s) (Lenormand2012.thetas s) }
        cacheSteps s = cache' "output/formulas/5steps/toy/lenormand2012"
                       (pure s)
 
steadyStateSteps :: Rand StdGen (Cached [Run])
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
          , SteadyState.distanceToData = absoluteError 0 . V.head
          }
        getRun :: (Int, SteadyState.S) -> Run
        getRun (i, s) = Run 
          { getAlgorithm = algo
          , getStep = i
          , getReplication = 1
          , getSample = fmap (\a -> (SteadyState.getWeight a, SteadyState.getTheta $ SteadyState.getSimulation $ SteadyState.getReady a)) (SteadyState.accepteds s) 
          }
        cacheSteps s = cache' "output/formulas/5steps/toy/steadyState"
                       (Cached.fromIO mempty s)

rootSquaredError :: Double -> Double -> Double
rootSquaredError expected x = sqrt ((x - expected) ** 2)

absoluteError :: Double -> Double -> Double
absoluteError expected x = abs (x - expected)

easyABCLenormand2012Steps :: Cached [Run]
easyABCLenormand2012Steps = traverse getRun (zip [1..] files)
  where getRun :: (Int, FilePath) -> Cached Run
        getRun (i,f) = source f (read i f)
        read :: Int -> FilePath -> Text -> Either Text Run
        read i f = bimap show (run i) . read1DSample f
        run :: Int -> (V.Vector (Weight, V.Vector Double)) -> Run
        run i s = Run { getAlgorithm = Lenormand2012 5000 0.1 0.01
                      , getStep = i
                      , getReplication = 1
                      , getSample = s }
        files = [ "output/easyABC/simulationResult/5steps/"      
                  <> "lenormand2012_5000_0.1_0.01_1_1.csv"
                , "output/easyABC/simulationResult/5steps/"      
                  <> "lenormand2012_5000_0.1_0.01_2_1.csv"
                , "output/easyABC/simulationResult/5steps/"      
                  <> "lenormand2012_5000_0.1_0.01_3_1.csv"
                , "output/easyABC/simulationResult/5steps/"      
                  <> "lenormand2012_5000_0.1_0.01_4_1.csv"
                , "output/easyABC/simulationResult/5steps/"      
                  <> "lenormand2012_5000_0.1_0.01_5_1.csv"
                ]
                
easyABCBeaumont2009Steps :: Cached [Run]
easyABCBeaumont2009Steps = traverse getRun (zip [1..] files)
  where getRun :: (Int, FilePath) -> Cached Run
        getRun (i,f) = source f (read i f)
        read :: Int -> FilePath -> Text -> Either Text Run
        read i f = bimap show (run i) . read1DSample f
        run :: Int -> (V.Vector (Weight, V.Vector Double)) -> Run
        run i s = Run { getAlgorithm = Beaumont2009 5000 2.0 0.01
                      , getStep = i
                      , getReplication = 1
                      , getSample = s }
        files = [ "output/easyABC/simulationResult/5steps/"
                  <> "beaumont2009_5000_2.00_0.01_1_1.csv"
                , "output/easyABC/simulationResult/5steps/"
                  <> "beaumont2009_5000_2.00_0.01_2_1.csv"
                , "output/easyABC/simulationResult/5steps/"
                  <> "beaumont2009_5000_2.00_0.01_3_1.csv"
                , "output/easyABC/simulationResult/5steps/"
                  <> "beaumont2009_5000_2.00_0.01_4_1.csv"
                , "output/easyABC/simulationResult/5steps/"
                  <> "beaumont2009_5000_2.00_0.01_5_1.csv"
                , "output/easyABC/simulationResult/5steps/"
                  <> "beaumont2009_5000_2.00_0.01_6_1.csv"
                , "output/easyABC/simulationResult/5steps/"
                  <> "beaumont2009_5000_2.00_0.01_7_1.csv"
                ]

histogramStep :: Run -> [(Double, Double)]
histogramStep = estPostDen (-10) 10 300 . fmap (second V.head) . V.toList . getSample

-- cacheHistogram :: FilePath -> Cached (V.Vector Double) -> Cached [(Double, Double)]
-- cacheHistogram path r = 
--   histogramStep <$> r
--   & cache path
--           (columns2 " ")
--           (bimap show identity . readHistogram path)

histogramsLenormand2012 :: Rand StdGen (Cached [[(Double, Double)]])
histogramsLenormand2012 = 
  lenormand2012Steps
  >>= return . (fmap . fmap) histogramStep
  >>= return . cache' "output/formulas/scaledHistogram/toy/lenormand2012" 

histogramsSteadyState :: Rand StdGen (Cached [[(Double, Double)]])
histogramsSteadyState = 
  steadyStateSteps
  >>= return . (fmap . fmap) histogramStep
  >>= return . cache' "output/formulas/scaledHistogram/toy/steadyState"

histogramsEasyABCLenormand2012 :: Cached [[(Double, Double)]]
histogramsEasyABCLenormand2012 = 
  easyABCLenormand2012Steps
  & (fmap . fmap) histogramStep
  & cache' "output/easyABC/scaledHistogram/toy/lenormand2012" 

histogramsEasyABCBeaumont2009 :: Cached [[(Double, Double)]]
histogramsEasyABCBeaumont2009 = 
  easyABCBeaumont2009Steps
  & (fmap . fmap) histogramStep
  & cache' "output/easyABC/scaledHistogram/toy/beaumont2009" 

histogramLenormand2012Step :: Int -> Rand StdGen (Cached ())
histogramLenormand2012Step i = do
  hs <- histogramsLenormand2012
  let cmh = List.lookup i . zip [1..] <$> hs 
  return $ sink
    ("output/formulas/scaledHistogram/toy/lenormand2012_" <> show i <> ".csv")
    (\mh -> case mh of
            Just h -> Right $ columns2 " " h 
            Nothing -> Left $ "No step " <> show i <> " for histogram Lenornand2012")
    cmh
        
histogramSteadyStateStep :: Int -> Rand StdGen (Cached ())
histogramSteadyStateStep i = do
  hs <- histogramsSteadyState
  let cmh = List.lookup i . zip [1..] <$> hs 
  return $ sink
    ("output/formulas/scaledHistogram/toy/steadyState_" <> show (i * 5000) <> ".csv")
    (\mh -> case mh of
            Just h -> Right $ columns2 " " h
            Nothing -> Left $ "No step " <> show i <> " for histogram SteadyState")
    cmh

histogramEasyABCLenormand2012Step :: Int -> Cached ()
histogramEasyABCLenormand2012Step i =
  let hs = histogramsEasyABCLenormand2012
      cmh = List.lookup i . zip [1..] <$> hs 
  in sink
    ("output/easyABC/scaledHistogram/toy/lenormand2012_" <> show i <> ".csv")
    (\mh -> case mh of
            Just h -> Right $ columns2 " " h 
            Nothing -> Left $ "No step " <> show i <> " for histogram EasyABCLenornand2012")
    cmh
    
histogramEasyABCBeaumont2009Step :: Int -> Cached ()
histogramEasyABCBeaumont2009Step i =
  let hs = histogramsEasyABCBeaumont2009
      cmh = List.lookup i . zip [1..] <$> hs 
  in sink
    ("output/easyABC/scaledHistogram/toy/beaumont2009_" <> show i <> ".csv")
    (\mh -> case mh of
            Just h -> Right $ columns2 " " h 
            Nothing -> Left $ "No step " <> show i <> " for histogram EasyABCBeaumont2009")
    cmh
        
figurePosteriorSteps :: Cached () 
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
                                <> "lenormand2012_1.csv")
    , ( "easyABC_lenormand2012_2", "output/easyABC/scaledHistogram/toy/"
                                <> "lenormand2012_2.csv")
    , ( "easyABC_lenormand2012_3", "output/easyABC/scaledHistogram/toy/"
                                <> "lenormand2012_3.csv")
    , ( "easyABC_lenormand2012_4", "output/easyABC/scaledHistogram/toy/"
                                <> "lenormand2012_4.csv")
    , ( "easyABC_lenormand2012_5", "output/easyABC/scaledHistogram/toy/"
                               <> "lenormand2012_5.csv")
    , ( "easyABC_beaumont2009_1", "output/easyABC/scaledHistogram/toy/"
                               <> "beaumont2009_1.csv")
    , ( "easyABC_beaumont2009_2", "output/easyABC/scaledHistogram/toy/"
                               <> "beaumont2009_2.csv")
    , ( "easyABC_beaumont2009_3", "output/easyABC/scaledHistogram/toy/"
                               <> "beaumont2009_3.csv")
    , ( "easyABC_beaumont2009_4", "output/easyABC/scaledHistogram/toy/"
                               <> "beaumont2009_4.csv")
    , ( "easyABC_beaumont2009_5", "output/easyABC/scaledHistogram/toy/"
                               <> "beaumont2009_7.csv")
    ]

buildSteps :: Rand StdGen (Cached ())
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
           $ liftA2 mappend (return $ histogramEasyABCLenormand2012Step 1) 
           $ liftA2 mappend (return $ histogramEasyABCLenormand2012Step 2)
           $ liftA2 mappend (return $ histogramEasyABCLenormand2012Step 3)
           $ liftA2 mappend (return $ histogramEasyABCLenormand2012Step 4)
           $ liftA2 mappend (return $ histogramEasyABCLenormand2012Step 5)
           $ liftA2 mappend (return $ histogramEasyABCBeaumont2009Step 1)
           $ liftA2 mappend (return $ histogramEasyABCBeaumont2009Step 2)
           $ liftA2 mappend (return $ histogramEasyABCBeaumont2009Step 3)
           $ liftA2 mappend (return $ histogramEasyABCBeaumont2009Step 4)
           $ liftA2 mappend (return $ histogramEasyABCBeaumont2009Step 5)
           $ liftA2 mappend (return $ histogramEasyABCBeaumont2009Step 6)
           $ liftA2 mappend (return $ histogramEasyABCBeaumont2009Step 7)
                            (return $ figurePosteriorSteps)

