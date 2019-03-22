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
         { _t = Lenormand2012.t r
         , _epsilon = Lenormand2012.epsilon r
         , _pAcc = Lenormand2012.pAcc r
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
      getStep MonAPMC.E = StepResult 0 0 0 mempty
      getStep MonAPMC.S{MonAPMC._s = s} = StepResult
        { _t = Lenormand2012.t s
        , _epsilon = Lenormand2012.epsilon s
        , _pAcc = Lenormand2012.pAcc s
        , _sample = V.zip (V.fromList $ LA.toList $ Lenormand2012.weights s)
              (V.fromList $ fmap V.fromList $ LA.toLists
                 $ Lenormand2012.thetas s)  }
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
 
easyABCLenormand2012Steps :: (Steps, Cached StepsResult)
easyABCLenormand2012Steps = (s,sr)
  where s = steps stepMax algo
        sr = fmap StepsResult $ traverse getStep (zip [1..] files)
        getStep :: (Int, FilePath)
                -> Cached StepResult
        getStep (i,f) = source f (read i f)
        algo = Lenormand2012 5000 0.1 0.01
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

histogramsEasyABCLenormand2012 :: Cached [(Int, [(Double, Double)])]
histogramsEasyABCLenormand2012 = 
  snd easyABCLenormand2012Steps
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
      len = Lenormand2012{getN=5000, getAlpha=0.1,
                          getPAccMin=0.01}
      moa1 = MonAPMC{ getN=5000
                    , getAlpha=0.1
                    , getPAccMin=0.01
                    , getStepSize = 1
                    , getParallel = 2}
      moa2 = MonAPMC{ getN=5000
                    , getAlpha=0.1
                    , getPAccMin=0.01
                    , getStepSize = 2
                    , getParallel = 1}
      lenEasyABC = _algorithm $ fst easyABCLenormand2012Steps
      beaEasyABC = _algorithm $ fst easyABCBeaumont2009Steps
      figData :: Compose (Rand StdGen) Cached [(Text, [(Int, [(Double, Double)])])]
      figData = liftA2 (<>)
                    (pure <$> ("APMC",) <$> histogramSteps len)
                $ liftA2 (<>)
                    (pure <$> ("MonAPMC StepSize 1 Parallel2",) <$>
                      histogramSteps moa1)
                $ liftA2 (<>)
                    (pure <$> ("MonAPMC StepSize 2 Parallel1",) <$>
                      histogramSteps moa2)
                $ liftA2 (<>)
                    (pure <$> ("EasyABC APMC",)
                          <$> Compose (pure histogramsEasyABCLenormand2012))
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

