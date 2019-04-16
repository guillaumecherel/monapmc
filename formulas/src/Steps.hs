{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Steps where

import Protolude 

import Data.Text (unpack, unlines)
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
import qualified ABC.Lenormand2012 as APMC
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
  { _t :: Int
  , _epsilon :: Double
  , _pAcc :: Double
  , _sample :: V.Vector (Run.Weight, V.Vector Double) }
  deriving (Show, Read)

data StepsResult = StepsResult
  { _steps :: [StepResult] }
  deriving (Show, Read)

stepsResult :: Steps -> Rand StdGen (IO StepsResult)
stepsResult Steps
  { _algorithm=APMC{ getN=n
                            , getNAlpha=nAlpha
                            , getPAccMin=pAccMin}} =
  let steps' :: Rand StdGen [APMC.S]
      steps' = APMC.scan p toyModel
      p = APMC.P
        { APMC.n = n
        , APMC.nAlpha = nAlpha
        , APMC.pAccMin = pAccMin
        , APMC.priorSample = toyPriorRandomSample
        , APMC.priorDensity = toyPrior
        , APMC.observed = V.singleton 0
        }
      getStep r = StepResult
         { _t = APMC.t r
         , _epsilon = APMC.epsilon r
         , _pAcc = APMC.pAcc r
         , _sample = V.zip (V.fromList $ LA.toList $ APMC.weights r)
                        (V.fromList $ fmap V.fromList $ LA.toLists
                          $ APMC.thetas r) }
  in return . StepsResult . fmap getStep <$> steps'
stepsResult Steps
  { _algorithm=MonAPMC{ getN=n
                      , getNAlpha=nAlpha
                      , getPAccMin=pAccMin
                      , getStepSize=stepSize
                      , getParallel=parallel
                      , getStopSampleSizeFactor=sf}} =
  let steps' :: RandT StdGen IO [MonAPMC.S (RandT StdGen IO)]
      steps' = MonAPMC.scanPar stepSize parallel p toyModel
      p = MonAPMC.P
          { MonAPMC._apmcP=APMC.P
              { APMC.n = n
              , APMC.nAlpha = nAlpha
              , APMC.pAccMin = pAccMin
              , APMC.priorSample = toyPriorRandomSample
              , APMC.priorDensity = toyPrior
              , APMC.observed = V.singleton 0
              }
          , MonAPMC._stopSampleSizeFactor=sf
          }
      getStep MonAPMC.E = StepResult 0 0 0 mempty
      getStep MonAPMC.S{MonAPMC._s = s} = StepResult
        { _t = APMC.t s
        , _epsilon = APMC.epsilon s
        , _pAcc = APMC.pAcc s
        , _sample = V.zip (V.fromList $ LA.toList $ APMC.weights s)
              (V.fromList $ fmap V.fromList $ LA.toLists
                 $ APMC.thetas s)  }
  in do
    g <- getSplit
    return $ StepsResult . fmap getStep <$> evalRandT steps' g
stepsResult Steps
  { _algorithm=SteadyState{}} = return $ return $ StepsResult mempty
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
 
easyABCAPMCSteps :: (Steps, Cached StepsResult)
easyABCAPMCSteps = (s,sr)
  where s = steps stepMax algo
        sr = fmap StepsResult $ traverse getStep (zip [1..] files)
        getStep :: (Int, FilePath)
                -> Cached StepResult
        getStep (i,f) = source f (read i f)
        algo = APMC 5000 500 0.01
        stepMax = 0
        read :: Int -> FilePath -> Text
             -> Either Text StepResult
        read i f = bimap show (StepResult i 0 0) . Run.read1DSample f
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
        sr = fmap StepsResult $ traverse getStep (zip [1..] files)
        getStep :: (Int, FilePath)
                -> Cached StepResult
        getStep (i, f) = source f (read i f)
        algo = Beaumont2009 5000 2.0 0.01
        stepMax = 0
        read :: Int -> FilePath -> Text
             -> Either Text StepResult
        read i f = bimap show (StepResult i 0 0) . Run.read1DSample f
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

---- Statistics over steps ----

l2Toy :: StepResult -> Double
l2Toy r = Statistics.l2Toy $ _sample r

histogramStep :: StepResult -> [(Double, Double)]
histogramStep = estPostDen (-10) 10 300 . fmap (second V.head) . V.toList . _sample

--------

histogramSteps :: Algorithm -> Compose (Rand StdGen) Cached [(Int, [(Double, Double)])]
histogramSteps algo =
  Compose
  $ fmap (cache' (histogramStepsCachePath algo))
  $ getCompose
  $ fmap ((,) <$> _t <*> histogramStep) . _steps
  <$> cachedStepsResult "output/formulas/cached" 100 algo

histogramStepsCachePath :: Algorithm -> FilePath
histogramStepsCachePath algo =
  "output/formulas/cached/scaledHistogram/toy/" <> algoFilename algo

histogramsEasyABCAPMC :: Cached [(Int, [(Double, Double)])]
histogramsEasyABCAPMC = 
  snd easyABCAPMCSteps
  & fmap _steps
  & (fmap . fmap) ((,) <$> _t <*> histogramStep)
  & cache' "output/easyABC/scaledHistogram/toy/lenormand2012" 

histogramsEasyABCBeaumont2009 :: Cached [(Int, [(Double, Double)])]
histogramsEasyABCBeaumont2009 = 
  snd easyABCBeaumont2009Steps
  & fmap _steps
  & (fmap . fmap) ((,) <$> _t <*> histogramStep)
  & cache' "output/easyABC/scaledHistogram/toy/beaumont2009"

fig :: Compose (Rand StdGen) Cached ()
fig =
  let outputPath = "report/5steps.png"
      len = APMC{getN=5000, getNAlpha=500,
                          getPAccMin=0.01}
      moa stepSize par = MonAPMC{ getN=5000
                    , getNAlpha=500
                    , getPAccMin=0.01
                    , getStepSize = stepSize
                    , getParallel = par
                    , getStopSampleSizeFactor = 5}
      lenEasyABC = _algorithm $ fst easyABCAPMCSteps
      beaEasyABC = _algorithm $ fst easyABCBeaumont2009Steps
      figData :: Compose (Rand StdGen) Cached [(Text, [(Int, [(Double, Double)])])]
      figData = liftA2 (<>)
                    (pure <$> ("APMC",) <$> histogramSteps len)
                $ liftA2 (<>)
                    (pure <$> ("MonAPMC StepSize 1 Parallel2",) <$>
                      histogramSteps (moa 1 2))
                $ liftA2 (<>)
                    (pure <$> ("MonAPMC StepSize 2 Parallel1",) <$>
                      histogramSteps (moa 2 1))
                $ liftA2 (<>)
                    (pure <$> ("EasyABC APMC",)
                          <$> Compose (pure histogramsEasyABCAPMC))
                    (pure <$> ("EasyABC Beaumont2009",)
                          <$> Compose (pure histogramsEasyABCBeaumont2009))
      gnuplotScript :: [(Text, [(Int, [(Double, Double)])])] -> Text
      gnuplotScript d =
          let rows = length d
              columns = maximum $ fmap (length . snd) d
              width = fromIntegral columns * 1.4 * 72 * 2.54
              height = fromIntegral rows * 1.4 * 72 * 2.54
          in unlines $
                [ "set terminal png truecolor size "
                   <> show width <> "," <> show height <> " font ',12'"
                , "set output '" <> outputPath <> "'"
                , "set xrange [-4:4]"
                , "set yrange [0:3]"
                , "set samples 500"
                , "set isosamples 500"
                , "normal(x, mean, var) = exp(- ((x - mean) ** 2.0) / (2.0 * var)) / sqrt(2.0 * pi * var)"
                , "posterior(x) = 0.5 * normal(x, 0.0, 1.0 / 100.0) + 0.5 * normal(x, 0.0, 1.0)"
                , "set multiplot layout " <> show rows <> "," <> show columns <> " rowsfirst"
                ]
                <> mconcat
                  (flip fmap d (\(algoLabel, d') ->
                    mconcat $ take columns $
                    flip fmap d' (\(step, hist) ->
                      [ "set title '" <> algoLabel <> " "
                        <> show step <> "'"
                      , "plot '-' w boxes t '' fs solid, \\"
                      , "       posterior(x) t '' w l lc 'black'"
                      ]
                      <> (flip fmap hist
                        (\(x,y) -> show x <> " " <> show y))
                      <> ["e"])
                    <> repeat ["set multiplot next"]))
                <> ["unset multiplot"]
  in liftCR
       (sinkIO (unpack outputPath)
           (gnuplotInline (Just $ unpack "report/5steps.gnuplot.script")
             . gnuplotScript))
       figData

buildSteps :: Rand StdGen (Cached ())
buildSteps = getCompose fig

