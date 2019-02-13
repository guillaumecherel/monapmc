{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Steps where

import Protolude 

import Data.Text (unpack)
import Formatting
import Data.Functor.Compose
import Data.Cached as Cached
import Control.Monad.Random.Lazy
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import System.Random (StdGen, mkStdGen)

import Algorithm
import Figure
import Model
import Run
import Statistics
import qualified ABC.Lenormand2012 as Lenormand2012
import qualified ABC.MonAPMC as MonAPMC
import qualified ABC.SteadyState as SteadyState
import qualified Util.SteadyState as SteadyState 

newtype Steps = Steps {_steps::[Run]}
  deriving (Show, Read)

steps :: Int -> Algorithm -> Rand StdGen (IO Steps)
steps stepMax algo@Lenormand2012{getN=n, getAlpha=alpha, getPAccMin=pAccMin} =
  let steps' :: Rand StdGen [(Int, Lenormand2012.S)]
      steps' = zip [1..stepMax] <$> Lenormand2012.scan p toyModel
      p = Lenormand2012.P
        { Lenormand2012.n = n
        , Lenormand2012.nAlpha = floor $ alpha * (fromIntegral $ n)
        , Lenormand2012.pAccMin = pAccMin
        , Lenormand2012.priorSample = toyPriorRandomSample
        , Lenormand2012.priorDensity = toyPrior
        , Lenormand2012.observed = V.singleton 0
        }
      getRun (i, r) = Run
        { _algorithm = algo
        , _stepCount = i
        , _sample = V.zip (V.fromList $ LA.toList $ Lenormand2012.weights r)
                          (V.fromList $ fmap V.fromList $ LA.toLists
                            $ Lenormand2012.thetas r) }
  in return . Steps . fmap getRun <$> steps'
-- steps stepMax algo@MonAPMCSeq{getN=n, getAlpha=alpha, getPAccMin=pAccMin} =
--   let steps' :: Rand StdGen [(Int, MonAPMC.S (Rand StdGen))]
--       steps' = zip [1..stepMax] <$> MonAPMC.scanSeq p toyModel
--       p = Lenormand2012.P
--         { Lenormand2012.n = n
--         , Lenormand2012.nAlpha = floor $ alpha * (fromIntegral $ n)
--         , Lenormand2012.pAccMin = pAccMin
--         , Lenormand2012.priorSample = toyPriorRandomSample
--         , Lenormand2012.priorDensity = toyPrior
--         , Lenormand2012.observed = V.singleton 0
--         }
--       getRun (i, (MonAPMC.E)) = Run algo i mempty
--       getRun (i, (MonAPMC.S _ s)) = Run
--         { _algorithm = algo
--         , _stepCount = i
--         , _sample = V.zip (V.fromList $ LA.toList $ Lenormand2012.weights s)
--                           (V.fromList $ fmap V.fromList $ LA.toLists
--                             $ Lenormand2012.thetas s) }
--   in return . Steps . fmap getRun <$> steps'
steps stepMax algo@MonAPMC{getN=n, getAlpha=alpha, getPAccMin=pAccMin, getStepSize=stepSize, getParallel=parallel} =
  let steps' :: RandT StdGen IO [(Int, MonAPMC.S (RandT StdGen IO))]
      steps' = zip [1..stepMax] <$> MonAPMC.scanPar stepSize parallel p toyModel
      p = Lenormand2012.P
        { Lenormand2012.n = n
        , Lenormand2012.nAlpha = floor $ alpha * (fromIntegral $ n)
        , Lenormand2012.pAccMin = pAccMin
        , Lenormand2012.priorSample = toyPriorRandomSample
        , Lenormand2012.priorDensity = toyPrior
        , Lenormand2012.observed = V.singleton 0
        }
      getRun (i, (MonAPMC.E)) = Run algo i mempty
      getRun (i, (MonAPMC.S _ s)) = Run
        { _algorithm = algo
        , _stepCount = i
        , _sample = V.zip (V.fromList $ LA.toList $ Lenormand2012.weights s)
                          (V.fromList $ fmap V.fromList $ LA.toLists
                            $ Lenormand2012.thetas s) }
  in do
    g <- getSplit
    return $ Steps . fmap getRun <$> evalRandT steps' g
steps stepMax algo@SteadyState{getN=n, getAlpha=alpha, getPAccMin=pAccMin, getParallel=par} =
  let steps' :: RandT StdGen IO [(Int, SteadyState.S)]
      steps' = zip needSteps <$> SteadyState.scanIndices needSteps ssr
      needSteps = [n, n * 2 .. n * stepMax]
      ssr = SteadyState.runner p model
      model (seed, xs) = return $ evalRand (toyModel xs) (mkStdGen seed)
      p = SteadyState.P
        { SteadyState.n = getN algo
        , SteadyState.nAlpha = floor $ alpha * (fromIntegral n)
        , SteadyState.pAccMin = pAccMin
        , SteadyState.parallel = par
        , SteadyState.priorSample = toyPriorRandomSample
        , SteadyState.priorDensity = toyPrior
        , SteadyState.distanceToData = absoluteError 0 . V.head
        }
      getRun :: (Int, SteadyState.S) -> Run
      getRun (i, s) = Run 
        { _algorithm = algo
        , _stepCount = i
        , _sample = fmap (\a -> (SteadyState.getWeight a, SteadyState.getTheta $ SteadyState.getSimulation $ SteadyState.getReady a)) (SteadyState.accepteds s)
        }
  in do
    g <- getSplit
    return $ fmap Steps $ (fmap . fmap) getRun $ evalRandT steps' g
steps _ Beaumont2009{} = return (return (Steps []))

cachedSteps :: Int -> Algorithm -> Compose (Rand StdGen) Cached Steps
cachedSteps stepMax algo =
  Compose $ fmap (cache' (cachedStepsPath stepMax algo) . fromIO mempty)
          $ steps stepMax algo

cachedStepsPath :: Int -> Algorithm -> FilePath
cachedStepsPath stepMax Lenormand2012{getN=n, getAlpha=alpha, getPAccMin=pAccMin} =
  unpack $ "output/formulas/steps/lenormand2012_"
             <> show stepMax <> "_"
             <> show n <> "_"
             <> sformat (fixed 2) alpha <> "_"
             <> sformat (fixed 2) pAccMin
-- cachedStepsPath stepMax MonAPMCSeq{getN=n, getAlpha=alpha, getPAccMin=pAccMin} =
--   unpack $ "output/formulas/steps/monAPMCSeq_"
--              <> show stepMax <> "_"
--              <> show n <> "_"
--              <> sformat (fixed 2) alpha <> "_"
--              <> sformat (fixed 2) pAccMin
cachedStepsPath stepMax MonAPMC{getN=n, getAlpha=alpha, getPAccMin=pAccMin, getStepSize=stepSize, getParallel=parallel} =
  unpack $ "output/formulas/steps/monAPMC_"
             <> show stepMax <> "_"
             <> show n <> "_"
             <> sformat (fixed 2) alpha <> "_"
             <> sformat (fixed 2) pAccMin <> "_"
             <> show stepSize <> "_"
             <> show parallel
cachedStepsPath stepMax Beaumont2009{getN=n, getEpsilonFrom=ef, getEpsilonTo=et} =
  unpack $ "output/formulas/steps/beaumont2009_"
             <> show stepMax <> "_"
             <> show n <> "_"
             <> sformat (fixed 2) ef <> "_"
             <> sformat (fixed 2) et 
cachedStepsPath stepMax SteadyState{getN=n, getAlpha=alpha, getPAccMin=pAccMin, getParallel=par} =
   unpack $ "output/formulas/steps/steadyState_"
             <> show stepMax <> "_"
             <> show n <> "_"
             <> sformat (fixed 2) alpha <> "_"
             <> sformat (fixed 2) pAccMin <> "_"
             <> show par
 
easyABCLenormand2012Steps :: Cached Steps
easyABCLenormand2012Steps = fmap Steps $ traverse getRun (zip [1..] files)
  where getRun :: (Int, FilePath) -> Cached Run
        getRun (i,f) = source f (read i f)
        read :: Int -> FilePath -> Text -> Either Text Run
        read i f = bimap show (run' i) . read1DSample f
        run' :: Int -> (V.Vector (Weight, V.Vector Double)) -> Run
        run' i s = Run { _algorithm = Lenormand2012 5000 0.1 0.01
                      , _stepCount = i
                      , _sample = s }
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
                
easyABCBeaumont2009Steps :: Cached Steps
easyABCBeaumont2009Steps = fmap Steps $ traverse getRun (zip [1..] files)
  where getRun :: (Int, FilePath) -> Cached Run
        getRun (i,f) = source f (read i f)
        read :: Int -> FilePath -> Text -> Either Text Run
        read i f = bimap show (run' i) . read1DSample f
        run' :: Int -> (V.Vector (Weight, V.Vector Double)) -> Run
        run' i s = Run { _algorithm = Beaumont2009 5000 2.0 0.01
                      , _stepCount = i
                      , _sample = s }
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
histogramStep = estPostDen (-10) 10 300 . fmap (second V.head) . V.toList . _sample

histogramSteps :: Algorithm -> Compose (Rand StdGen) Cached [[(Double, Double)]]
histogramSteps algo =
  Compose
  $ fmap (cache' (histogramStepsCachePath algo))
  $ getCompose
  $ fmap histogramStep . _steps
  <$> cachedSteps 100 algo

histogramStepsCachePath :: Algorithm -> FilePath
histogramStepsCachePath SteadyState{} =
  "output/formulas/scaledHistogram/toy/steadyState"
-- histogramStepsCachePath MonAPMCSeq{} =
--   "output/formulas/scaledHistogram/toy/monAPMCSeq"
histogramStepsCachePath MonAPMC{} =
  "output/formulas/scaledHistogram/toy/monAPMC"
histogramStepsCachePath Lenormand2012{} =
  "output/formulas/scaledHistogram/toy/lenormand2012"
histogramStepsCachePath Beaumont2009{} =
  "output/formulas/scaledHistogram/toy/beaumont2009"

histogramsEasyABCLenormand2012 :: Cached [[(Double, Double)]]
histogramsEasyABCLenormand2012 = 
  easyABCLenormand2012Steps
  & fmap _steps
  & (fmap . fmap) histogramStep
  & cache' "output/easyABC/scaledHistogram/toy/lenormand2012" 

histogramsEasyABCBeaumont2009 :: Cached [[(Double, Double)]]
histogramsEasyABCBeaumont2009 = 
  easyABCBeaumont2009Steps
  & fmap _steps
  & (fmap . fmap) histogramStep
  & cache' "output/easyABC/scaledHistogram/toy/beaumont2009"

fig :: Compose (Rand StdGen) Cached ()
fig =
  let len = histogramSteps Lenormand2012{getN=5000, getAlpha=0.1,
                                         getPAccMin=0.01}
--       mas = histogramSteps MonAPMCSeq{getN=5000, getAlpha=0.1,
--                                          getPAccMin=0.01}
      moa = histogramSteps MonAPMC{ getN=5000
                                  , getAlpha=0.1
                                  , getPAccMin=0.01
                                  , getStepSize = 1
                                  , getParallel = 2}
      ste = histogramSteps SteadyState { getN = 5000
                                       , getAlpha = 0.1
                                       , getPAccMin = 0.01
                                       , getParallel = 1}
      lenEasyABC = histogramsEasyABCLenormand2012
      beaEasyABC = histogramsEasyABCBeaumont2009
      gpData :: [[(Double, Double)]] -> GnuplotData
      gpData = gnuplotData2 fst snd
      gpInputFile :: FilePath 
                  -> Compose (Rand StdGen) Cached [[(Double, Double)]]
                  -> Compose (Rand StdGen) Cached ()
      gpInputFile path hs =
        Compose $ sink path (Right . gnuplotDataText . gpData)
          <$> getCompose hs
      lenHistPath = "output/formulas/scaledHistogram/toy/lenormand2012.csv"
--      masHistPath = "output/formulas/scaledHistogram/toy/monAPMCSeq.csv"
      moaHistPath = "output/formulas/scaledHistogram/toy/monAPMC.csv"
      steHistPath = "output/formulas/scaledHistogram/toy/steadyState.csv"
      lenEasyABCHistPath = "output/easyABC/scaledHistogram/toy/lenormand2012.csv"
      beaEasyABCHistPath = "output/easyABC/scaledHistogram/toy/beaumont2009.csv"
      gp :: Cached ()
      gp = gnuplot "report/5steps.png" "report/5steps.gnuplot"
            [ ( "formulas_lenormand2012"
              , "output/formulas/scaledHistogram/toy/"
                   <>  "lenormand2012.csv")
--             , ( "formulas_monAPMCSeq"
--               , "output/formulas/scaledHistogram/toy/"
--                    <> "monAPMCSeq.csv")
            , ( "formulas_monAPMC"
              , "output/formulas/scaledHistogram/toy/"
                   <> "monAPMC.csv")
            , ( "formulas_steadyState"
              , "output/formulas/scaledHistogram/toy/"
                   <> "steadyState.csv")
            , ( "easyABC_lenormand2012"
              , "output/easyABC/scaledHistogram/toy/"
                   <> "lenormand2012.csv")
            , ( "easyABC_beaumont2009"
              , "output/easyABC/scaledHistogram/toy/"
                   <> "beaumont2009.csv")
            ]
  in foldr (liftC2 (<>)) (Compose (pure gp))
       [ gpInputFile lenHistPath len
--        , gpInputFile masHistPath mas
       , gpInputFile moaHistPath moa
       , gpInputFile steHistPath ste
       , gpInputFile lenEasyABCHistPath (Compose $ pure lenEasyABC)
       , gpInputFile beaEasyABCHistPath (Compose $ pure beaEasyABC)
       ]

liftC :: (Cached a -> Cached b) 
      -> Compose (Rand StdGen) Cached a
      -> Compose (Rand StdGen) Cached b
liftC f = Compose . liftA f . getCompose

liftC2 :: (Cached a -> Cached b -> Cached c)
       -> Compose (Rand StdGen) Cached a
       -> Compose (Rand StdGen) Cached b
       -> Compose (Rand StdGen) Cached c
liftC2 f a b = Compose $ liftA2 f (getCompose a) (getCompose b)

buildSteps :: Rand StdGen (Cached ())
buildSteps = getCompose fig

