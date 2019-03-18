{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Steps where

import Protolude 

import Data.Text (unpack)
import Data.Csv
import Data.Functor.Compose
import Data.Cached as Cached
import Control.Monad.Random.Lazy
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import System.Random (StdGen, mkStdGen)
import System.FilePath ((</>))

import Algorithm
import Figure
import Model
import qualified Run
import Statistics
import qualified ABC.Lenormand2012 as Lenormand2012
import qualified ABC.MonAPMC as MonAPMC
import qualified ABC.SteadyState as SteadyState
import Util
import Util.CSV
import qualified Util.SteadyState as SteadyState 

data Steps = Steps { _algorithm :: Algorithm
                   , _stepMax :: Int}
  deriving (Show, Read)

steps :: Int -> Algorithm -> Steps
steps = flip Steps

data StepResult = StepResult
  { _epsilon :: Double
  , _sample :: V.Vector (Run.Weight, V.Vector Double) }
  deriving (Show, Read)

data StepsResult = StepsResult
  { _steps :: [StepResult] }
  deriving (Show, Read)

stepsResult :: Steps -> Rand StdGen (IO StepsResult)
stepsResult Steps
  { _algorithm=Lenormand2012{ getN=n
                            , getAlpha=alpha
                            , getPAccMin=pAccMin}} =
  let steps' :: Rand StdGen [Lenormand2012.S]
      steps' = Lenormand2012.scan p toyModel
      p = Lenormand2012.P
        { Lenormand2012.n = n
        , Lenormand2012.nAlpha = floor $ alpha * (fromIntegral $ n)
        , Lenormand2012.pAccMin = pAccMin
        , Lenormand2012.priorSample = toyPriorRandomSample
        , Lenormand2012.priorDensity = toyPrior
        , Lenormand2012.observed = V.singleton 0
        }
      getStep r = StepResult
         { _epsilon = Lenormand2012.epsilon r
         , _sample = V.zip (V.fromList $ LA.toList $ Lenormand2012.weights r)
                        (V.fromList $ fmap V.fromList $ LA.toLists
                          $ Lenormand2012.thetas r) }
  in return . StepsResult . fmap getStep <$> steps'
stepsResult Steps
  { _algorithm=MonAPMC{ getN=n
                      , getAlpha=alpha
                      , getPAccMin=pAccMin
                      , getStepSize=stepSize
                      , getParallel=parallel}} =
  let steps' :: RandT StdGen IO [MonAPMC.S (RandT StdGen IO)]
      steps' = MonAPMC.scanPar stepSize parallel p toyModel
      p = Lenormand2012.P
        { Lenormand2012.n = n
        , Lenormand2012.nAlpha = floor $ alpha * (fromIntegral $ n)
        , Lenormand2012.pAccMin = pAccMin
        , Lenormand2012.priorSample = toyPriorRandomSample
        , Lenormand2012.priorDensity = toyPrior
        , Lenormand2012.observed = V.singleton 0
        }
      getStep (MonAPMC.E) = StepResult 0 mempty
      getStep (MonAPMC.S _ s) = StepResult
        { _epsilon = Lenormand2012.epsilon s
        , _sample = V.zip (V.fromList $ LA.toList $ Lenormand2012.weights s)
              (V.fromList $ fmap V.fromList $ LA.toLists
                 $ Lenormand2012.thetas s)  }
  in do
    g <- getSplit
    return $ StepsResult . fmap getStep <$> evalRandT steps' g
stepsResult Steps
  { _stepMax=stepMax
  , _algorithm=SteadyState{ getN=n
                          , getAlpha=alpha
                          , getPAccMin=pAccMin
                          , getParallel=par}} =
  let steps' :: RandT StdGen IO [SteadyState.S]
      steps' = SteadyState.scanIndices needSteps ssr
      needSteps = [n, n * 2 .. n * stepMax]
      ssr = SteadyState.runner p model
      model (seed, xs) = return $ evalRand (toyModel xs) (mkStdGen seed)
      p = SteadyState.P
        { SteadyState.n = n
        , SteadyState.nAlpha = floor $ alpha * (fromIntegral n)
        , SteadyState.pAccMin = pAccMin
        , SteadyState.parallel = par
        , SteadyState.priorSample = toyPriorRandomSample
        , SteadyState.priorDensity = toyPrior
        , SteadyState.distanceToData = Run.absoluteError 0 . V.head
        }
      getStep s = StepResult
        { _epsilon = SteadyState.epsilon s
        , _sample = fmap (\a -> (SteadyState.getWeight a, SteadyState.getTheta $ SteadyState.getSimulation $ SteadyState.getReady a)) (SteadyState.accepteds s) }
  in do
    g <- getSplit
    return $ StepsResult . fmap getStep <$> evalRandT steps' g
stepsResult Steps
  { _algorithm=Beaumont2009{}} = return $ return $ StepsResult mempty

cachedStepsResult :: FilePath -> Int -> Algorithm -> Compose (Rand StdGen) Cached StepsResult
cachedStepsResult rootDir stepMax algo =
  Compose $ fmap (cache' (rootDir </> cachedStepsPath' stepMax algo) . fromIO mempty)
          $ stepsResult (steps stepMax algo)

cachedStepsPath :: Steps -> FilePath
cachedStepsPath Steps{_stepMax=stepMax, _algorithm=algo} =
  cachedStepsPath' stepMax algo

cachedStepsPath' :: Int -> Algorithm -> FilePath
cachedStepsPath' stepMax algo =
  (unpack $ "steps_" <> show stepMax <> "/" ) <> algoFilename algo
 
easyABCLenormand2012Steps :: (Steps, Cached StepsResult)
easyABCLenormand2012Steps = (s,sr)
  where s = steps stepMax algo
        sr = fmap StepsResult $ traverse getStep files
        getStep :: FilePath
                -> Cached StepResult
        getStep f = source f (read f)
        algo = Lenormand2012 5000 0.1 0.01
        stepMax = 0
        read :: FilePath -> Text
             -> Either Text StepResult
        read f = bimap show (StepResult 0) . Run.read1DSample f
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
                
easyABCBeaumont2009Steps :: (Steps, Cached StepsResult)
easyABCBeaumont2009Steps = (s,sr)
  where s = steps stepMax algo
        sr = fmap StepsResult $ traverse getStep files
        getStep :: FilePath
                -> Cached StepResult
        getStep f = source f (read f)
        algo = Beaumont2009 5000 2.0 0.01
        stepMax = 0
        read :: FilePath -> Text
             -> Either Text StepResult
        read f = bimap show (StepResult 0) . Run.read1DSample f
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

histogramStep :: StepResult -> [(Double, Double)]
histogramStep = estPostDen (-10) 10 300 . fmap (second V.head) . V.toList . _sample

histogramSteps :: Algorithm -> Compose (Rand StdGen) Cached [[(Double, Double)]]
histogramSteps algo =
  Compose
  $ fmap (cache' (histogramStepsCachePath algo))
  $ getCompose
  $ fmap histogramStep . _steps 
  <$> cachedStepsResult "output/formulas/cached" 100 algo

histogramStepsCachePath :: Algorithm -> FilePath
histogramStepsCachePath algo =
  "output/formulas/cached/scaledHistogram/toy/" <> algoFilename algo

histogramsEasyABCLenormand2012 :: Cached [[(Double, Double)]]
histogramsEasyABCLenormand2012 = 
  snd easyABCLenormand2012Steps
  & fmap _steps
  & (fmap . fmap) histogramStep
  & cache' "output/easyABC/scaledHistogram/toy/lenormand2012" 

histogramsEasyABCBeaumont2009 :: Cached [[(Double, Double)]]
histogramsEasyABCBeaumont2009 = 
  snd easyABCBeaumont2009Steps
  & fmap _steps
  & (fmap . fmap) histogramStep
  & cache' "output/easyABC/scaledHistogram/toy/beaumont2009"

fig :: Compose (Rand StdGen) Cached ()
fig =
  let csvPath = "output/formulas/figures_data/steps.csv"
      len = Lenormand2012{getN=5000, getAlpha=0.1,
                          getPAccMin=0.01}
      moa1 = MonAPMC{ getN=5000
                    , getAlpha=0.1
                    , getPAccMin=0.01
                    , getStepSize = 1
                    , getParallel = 2}
      moa2 = MonAPMC{ getN=5000
                    , getAlpha=0.5
                    , getPAccMin=0.01
                    , getStepSize = 2
                    , getParallel = 1}
      lenEasyABC = _algorithm $ fst easyABCLenormand2012Steps
      beaEasyABC = _algorithm $ fst easyABCBeaumont2009Steps
      csv :: Compose (Rand StdGen) Cached ()
      csv = liftCR (csvSink csvPath ["algorithm", "step", "theta", "density", "theoretical"])
              figData
      figData :: Compose (Rand StdGen) Cached [(Text, Int, Double, Double, Double)]
      figData = fmap concat
                $ (fmap . fmap)
                  (\(algoText, (step, hist)) -> fmap
                      (\(theta,density) -> (algoText,step,theta,density,toyPosterior 0 theta))
                      hist )
                $ liftA2 (<>)
                    (("APMC",) <<$>> (zip [1..] <$> histogramSteps len))
                $ liftA2 (<>)
                    (("MonAPMC\nStepSize 1\nParallel2",) <<$>> (zip [1..] <$> histogramSteps moa1))
                $ liftA2 (<>)
                    (("MonAPMC\nStepSize 2\nParallel1",) <<$>> (zip [1..] <$> histogramSteps moa2))
                $ liftA2 (<>)
                    (("EasyABC APMC",)
                      <<$>> (zip [1..]
                         <$> Compose (pure histogramsEasyABCLenormand2012)))
                    (("EasyABC Beaumont2009",)
                        <<$>> (zip [1..]
                          <$> Compose (pure histogramsEasyABCBeaumont2009)))
  in csv

buildSteps :: Rand StdGen (Cached ())
buildSteps = getCompose fig

