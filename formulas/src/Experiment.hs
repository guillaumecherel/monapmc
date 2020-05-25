{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Experiment where

import Protolude


import qualified ABC.Lenormand2012 as APMC
import qualified ABC.MonAPMC as MonAPMC
import qualified ABC.SteadyState as SteadyState
import qualified Control.Foldl as Fold
import           Control.Foldl (Fold)
import           Control.Monad.Primitive
import           Control.Monad.Random.Lazy
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Text (unpack)
import qualified Data.Vector as Vector
import           Data.Vector (Vector)
import           Formatting
import qualified Model 
import           Model (Model(..))
import qualified Numeric.LinearAlgebra as LA
import           Util.DataSet (DataSet(..))
import           Util (strictlyPositive)
import qualified Util.DataSet as DataSet
import           Util.Duration (Duration)
import qualified Util.Duration as Duration
import           Util.Repli (Repli(..))
import qualified Util.Repli as Repli
import           Util.Sample (Sample(..))
import qualified Util.Sample as Sample
import qualified Util.Figure as UFig
import qualified Statistics
import           Text.Pretty.Simple (pShow, pShowNoColor)
import qualified Util.SteadyState as SteadyState 

---- Simulations Specification ----

data Algorithm =
  APMC
    { nGen :: Int
    , nAlpha :: Int
    , pAccMin :: Double
    , parallel :: Int
    }
  | MonAPMC
    { nGen :: Int
    , nAlpha :: Int
    , pAccMin :: Double
    , stepSize :: Int
    , parallel :: Int
    , stopSampleSize :: Int
    }
  | Beaumont2009
    { n :: Int
    , epsilonFrom :: Double
    , epsilonTo :: Double
    }
  | SteadyState
    { n :: Int
    , alpha :: Double
    , pAccMin :: Double
    , parallel :: Int
    }
  deriving (Show, Read, Eq, Ord)

algoFilename :: Algorithm -> FilePath
algoFilename APMC{nGen, nAlpha, pAccMin, parallel} =
   unpack $ "apmc_"
             <> show nGen <> "_"
             <> sformat (fixed 2) nAlpha <> "_"
             <> sformat (fixed 2) pAccMin <> "_"
             <> show parallel
algoFilename MonAPMC{nGen, nAlpha, pAccMin, stepSize, parallel, stopSampleSize} =
   unpack $ "monAPMC_"
             <> show nGen <> "_"
             <> show nAlpha <> "_"
             <> sformat (fixed 2) pAccMin <> "_"
             <> show stepSize <> "_"
             <> show parallel <> "_"
             <> show stopSampleSize
algoFilename SteadyState{n, alpha, pAccMin, parallel} =
   unpack $ "steadyState_"
             <> show n <> "_"
             <> sformat (fixed 2) alpha <> "_"
             <> sformat (fixed 2) pAccMin <> "_"
             <> show parallel
algoFilename Beaumont2009{n, epsilonFrom, epsilonTo} =
   unpack $ "beaumont2009"
             <> show n <> "_"
             <> sformat (fixed 2) epsilonFrom <> "_"
             <> sformat (fixed 2) epsilonTo

getAlgoNGen :: Algorithm -> Int
getAlgoNGen APMC {nGen} = nGen
getAlgoNGen MonAPMC {nGen} = nGen
getAlgoNGen SteadyState{} = undefined
getAlgoNGen Beaumont2009{} = undefined

getAlgoNAlpha :: Algorithm -> Int
getAlgoNAlpha APMC {nAlpha} = nAlpha
getAlgoNAlpha MonAPMC {nAlpha} = nAlpha
getAlgoNAlpha SteadyState{} = undefined
getAlgoNAlpha Beaumont2009{} = undefined

getAlgoName :: Algorithm -> Text
getAlgoName APMC{} = "APMC"
getAlgoName MonAPMC{} = "MonAPMC"
getAlgoName SteadyState{} = "SteadyState"
getAlgoName Beaumont2009{} = "Beaumont"

getAlgoParallel :: Algorithm -> Int
getAlgoParallel APMC{parallel} = parallel
getAlgoParallel MonAPMC{parallel} = parallel
getAlgoParallel SteadyState{parallel} = parallel
getAlgoParallel Beaumont2009{} = undefined

data Simulation = Simulation Algorithm Model Int
  -- Simulation algo model stepMax
  deriving (Show, Read)

getSimuAlgo :: Simulation -> Algorithm
getSimuAlgo (Simulation a _ _) = a

getSimuModel :: Simulation -> Model
getSimuModel (Simulation _ m _) = m


---- Simulation Run ----

data Run = Run Algorithm Model RunResult
  -- Run algo model runResult
  deriving (Show, Read)

type Weight = Double

run :: Simulation -> RandT StdGen IO Run
run (Simulation algo model stepMax) =
  Run algo model <$> runResult stepMax algo model

getRunAlgo :: Run -> Algorithm
getRunAlgo (Run algo _ _) = algo

getRunModel :: Run -> Model
getRunModel (Run _ model _) = model

getRunRunResult :: Run -> RunResult
getRunRunResult (Run _ _ rr) = rr

getRunSample :: Run -> Vector (Weight, Vector Double)
getRunSample (Run _ _ runRes) = getRunResultSample runRes

getRunSteps :: Run -> Int
getRunSteps (Run _ _ runRes) = getRunResultSteps runRes

data RunResult = RunResult (Duration, Duration) Int (Vector (Weight, Vector Double))
  -- RunResult (algoDuration, simDuration) nSteps sample
  deriving (Show, Read)

runResult :: Int -> Algorithm -> Model -> RandT StdGen IO RunResult
runResult stepMax APMC{nGen, nAlpha, pAccMin} model =
  let res :: RandT StdGen IO ((Duration, Duration), APMC.S)
      res = APMC.runPar 1 p (Model.model model)
      p = APMC.P
        { APMC.nGen = nGen
        , APMC.nAlpha = nAlpha
        , APMC.pAccMin = pAccMin
        , APMC.priorSample = Model.priorRandomSample model
        , APMC.priorDensity = Model.prior model
        , APMC.observed = Vector.singleton 0
        , APMC.stepMax = strictlyPositive stepMax
        }
      getRun (dur, r) = RunResult dur (APMC.t r)
        $ Vector.zip (Vector.fromList $ LA.toList $ APMC.weights r)
                          (Vector.fromList $ fmap Vector.fromList $ LA.toLists
                            $ APMC.thetas r)
  in getRun <$> res
runResult
  stepMax
  MonAPMC{nGen ,nAlpha ,pAccMin ,stepSize ,parallel ,stopSampleSize}
  model =
  let result :: RandT StdGen IO ((Duration, Duration), MonAPMC.S (Rand StdGen))
      result = MonAPMC.runPar stepSize parallel p (Model.model model)
      p = MonAPMC.P
          { MonAPMC._apmcP=APMC.P
              { APMC.nGen = nGen
              , APMC.nAlpha = nAlpha
              , APMC.pAccMin = pAccMin
              , APMC.priorSample = Model.priorRandomSample model
              , APMC.priorDensity = Model.prior model
              , APMC.observed = Vector.singleton 0
              , APMC.stepMax = strictlyPositive stepMax
              }
          , MonAPMC._stopSampleSize=stopSampleSize
          }
      getRun (dur, MonAPMC.E) = RunResult dur 0 mempty
      getRun (dur, MonAPMC.S{MonAPMC._s = s}) = RunResult
        dur
        (APMC.t s)
        $ Vector.zip (Vector.fromList $ LA.toList $ APMC.weights s)
                          (Vector.fromList $ fmap Vector.fromList $ LA.toLists
                            $ APMC.thetas s)
  in getRun <$> result
-- runResult stepMax SteadyState{n, alpha, pAccMin, parallel} model =
--   let step :: RandT StdGen IO SteadyState.S
--       step = SteadyState.runN (stepMax * n) ssr
--       ssr = SteadyState.runner p ((fmap . fmap) snd $ model')
--       model' (seed, xs) = return $ evalRand (Model.model model xs) (mkStdGen seed)
--       p :: SteadyState.P (RandT StdGen IO)
--       p = SteadyState.P
--         { SteadyState.n = n
--         , SteadyState.nAlpha = floor $ alpha * (fromIntegral $ n)
--         , SteadyState.pAccMin = pAccMin
--         , SteadyState.parallel = parallel
--         , SteadyState.priorSample = Model.priorRandomSample model
--         , SteadyState.priorDensity = Model.prior model
--         , SteadyState.distanceToData = Statistics.absoluteError 0 . Vector.head
--         }
--       getRun r = RunResult 
--         (SteadyState.curStep r)
--         $ fmap (\a -> (SteadyState.getWeight a, SteadyState.getTheta $ SteadyState.getSimulation $ SteadyState.getReady a)) (SteadyState.accepteds r)
--   in getRun <$> step
runResult stepMax SteadyState{n, alpha, pAccMin, parallel} model =
  return
    (RunResult
      (Duration.fromPicoSeconds 0, Duration.fromPicoSeconds 0)
      0
      mempty)
runResult _ Beaumont2009{} _ = 
  return
    (RunResult
      (Duration.fromPicoSeconds 0, Duration.fromPicoSeconds 0)
      0
      mempty)

getRunResultSample :: RunResult -> Vector (Weight, Vector Double)
getRunResultSample (RunResult _ _ sample) = sample

getRunResultSteps :: RunResult -> Int
getRunResultSteps (RunResult _ steps _) = steps

getRunResultAlgoTime :: RunResult -> Duration
getRunResultAlgoTime (RunResult (algoTime, _) _ _) = algoTime

getRunResultSimTime :: RunResult -> Duration
getRunResultSimTime (RunResult (_, simTime) _ _) = simTime

getRunResultTotalTime :: RunResult -> Duration
getRunResultTotalTime step = getRunResultAlgoTime step + getRunResultSimTime step


---- Simulation Steps ----

data Steps = Steps Simulation [Step]
  -- Steps simulation steps
  deriving (Show, Read)

steps :: Simulation -> RandT StdGen IO Steps
steps s@(Simulation algo model stepMax) =
  Steps s <$> stepsResult stepMax algo model

stepsResult :: Int -> Algorithm -> Model -> RandT StdGen IO [Step]
stepsResult stepMax APMC{nGen, nAlpha, pAccMin, parallel} model =
  let steps' :: RandT StdGen IO [((Duration, Duration), APMC.S)]
      steps' = take stepMax <$> APMC.scanPar parallel p (Model.model model)
      p = APMC.P
        { APMC.nGen = nGen
        , APMC.nAlpha = nAlpha
        , APMC.pAccMin = pAccMin
        , APMC.priorSample = Model.priorRandomSample model
        , APMC.priorDensity = Model.prior model
        , APMC.observed = Vector.singleton 0
        , APMC.stepMax = strictlyPositive stepMax
        }
      getStep (dur, r) = Step
         dur
         (APMC.t r)
         (APMC.epsilon r)
         (APMC.pAcc r)
         (Vector.zip (Vector.fromList $ LA.toList $ APMC.weights r)
                        (Vector.fromList $ fmap Vector.fromList $ LA.toLists
                          $ APMC.thetas r))
  in getStep <<$>> steps'
stepsResult
  stepMax
  MonAPMC {nGen, nAlpha, pAccMin, stepSize, parallel , stopSampleSize}
  model =
  let steps' :: RandT StdGen IO [((Duration, Duration), MonAPMC.S (Rand StdGen))]
      steps' = take stepMax
        <$> MonAPMC.scanPar stepSize parallel p (Model.model model)
      p = MonAPMC.P
          { MonAPMC._apmcP=APMC.P
              { APMC.nGen = nGen
              , APMC.nAlpha = nAlpha
              , APMC.pAccMin = pAccMin
              , APMC.priorSample = Model.priorRandomSample model
              , APMC.priorDensity = Model.prior model
              , APMC.observed = Vector.singleton 0
              , APMC.stepMax = strictlyPositive stepMax
              }
          , MonAPMC._stopSampleSize=stopSampleSize
          }
      getStep (dur, MonAPMC.E) = Step dur 0 0 0 mempty
      getStep (dur, MonAPMC.S{MonAPMC._s = s}) = Step
        dur
        ( APMC.t s)
        ( APMC.epsilon s)
        ( APMC.pAcc s)
        ( Vector.zip (Vector.fromList $ LA.toList $ APMC.weights s)
              (Vector.fromList $ fmap Vector.fromList $ LA.toLists
                 $ APMC.thetas s))
  in getStep <<$>> steps'
stepsResult _ SteadyState{} _ = return mempty
stepsResult _ Beaumont2009{} _ = return mempty

getStepsSimulation :: Steps -> Simulation
getStepsSimulation (Steps x _) = x

getSteps :: Steps -> [Step]
getSteps (Steps _ steps) = steps
 
data Step = Step (Duration, Duration) Int Double Double (Vector (Weight, Vector Double))
  -- Step (realTime, simulatedTime) t epsilon pAcc sample
  deriving (Show, Read)

getStepT :: Step -> Int
getStepT (Step _ t _ _ _) = t

getStepSample :: Step -> Vector (Weight, Vector Double)
getStepSample (Step _ _ _ _ sample) = sample

getStepAlgoTime :: Step -> Duration
getStepAlgoTime (Step (algoTime, _) _ _ _ _) = algoTime

getStepSimTime :: Step -> Duration
getStepSimTime (Step (_, simTime) _ _ _ _) = simTime

getStepTotalTime :: Step -> Duration
getStepTotalTime step = getStepAlgoTime step + getStepSimTime step

peekSteps :: Steps -> Text
peekSteps (Steps (Simulation algo model stepMax) steps) =
  "(Steps"
  <> " algo=" <> show algo
  <> " model=" <> show model
  <> " stepMax=" <> show stepMax
  <> " steps=" <> peekList 3 peekStep steps

peekStep :: Step -> Text
peekStep (Step dur t epsilon pAcc sample) =
  "(Step"
  <> " duration=" <> show dur
  <> " t=" <> show t
  <> " epsilon=" <> show epsilon
  <> " pAcc=" <> show pAcc
  <> " sample=" <> peekVector 3 show sample
  <> ")"

peekVector :: Int -> (a -> Text) -> Vector a -> Text
peekVector i peek v =
  "(Vector "
  <> peekList i peek (Vector.toList v)
  <> ")"

peekList :: Int -> (a -> Text) -> [a] -> Text
peekList i peek v =
  "Length " <> show len <> " ["
  <> (mconcat . intersperse ", " . fmap peek . take i $ v)
  <> if i < len then "..." else ""
  <> "]"
  where
    len = length v




---- Simulation Repli Run ----

repliRun :: Int -> Simulation -> RandT StdGen IO (Repli Run)
repliRun n sim = Repli.repliM n $ run sim




---- Simulation Repli Steps ----

repliSteps :: Int -> Simulation -> RandT StdGen IO (Repli Steps)
repliSteps n sim = Repli.repliM n $ steps sim



---- Simulation Comp ----

data Comp = Comp 
  { getCompNGen :: Int
  , getCompNAlpha :: Int 
  , getCompParallel :: Int 
  , getCompStepMax :: Int 
  , getCompBiasFactor :: Double 
  , getCompMeanRunTime :: Double 
  , getCompVarRunTime :: Double 
  , getCompApmcRun :: Run
  , getCompMonApmcRun :: Run
  , getCompModel :: Model
  }
  deriving (Show, Read)

comp :: Int -> Int -> Double -> Int -> Int -> Double -> Double -> Double 
     -> RandT StdGen IO Comp
comp nGen nAlpha pAccMin parallel stepMax biasFactor meanRunTime varRunTime = 
  Comp nGen nAlpha parallel stepMax biasFactor meanRunTime varRunTime
  <$> runApmc <*> runMonApmc <*> pure model
  where
    runApmc = run (Simulation algoApmc model stepMaxApmc)
    runMonApmc = run (Simulation algoMonApmc model stepMaxMonApmc)
    algoApmc = APMC nGen nAlpha pAccMin parallel
    stepMaxApmc = stepMax
    algoMonApmc = MonAPMC (nGen `div` parallel) nAlpha pAccMin stepSize parallel (nGen + nAlpha)
    stepSize = 1
    stepMaxMonApmc = stepMax * parallel 
    model = ToyTimeBias biasFactor 0 meanRunTime varRunTime

getCompSampleApmc = getRunSample . getCompApmcRun
getCompSampleMonApmc = getRunSample . getCompMonApmcRun
getCompTimeApmc = getRunResultTotalTime . getRunRunResult . getCompApmcRun
getCompTimeMonApmc = getRunResultTotalTime . getRunRunResult . getCompMonApmcRun

---- Stats histo ----

postDensityEstimate Toy = Statistics.estPostDen (-10) 10 300
postDensityEstimate (ToyTimeVar _ _) = Statistics.estPostDen (-10) 10 300
postDensityEstimate (ToyTimeBias _ _ _ _) = Statistics.estPostDen (-2) 2 20

histogramRun :: Run -> DataSet (Double, Double)
histogramRun run =
    DataSet
  $ postDensityEstimate (getRunModel run)
  $ fmap (second Vector.head)
  $ Vector.toList
  $ getRunSample run

histogramStep :: Model -> Step -> DataSet (Double, Double)
histogramStep model step =
    DataSet
  $ postDensityEstimate model
  $ fmap (second Vector.head)
  $ Vector.toList (getStepSample step)

histogramSteps :: Steps -> [DataSet (Double, Double)]
histogramSteps steps =
  histogramStep (getSimuModel . getStepsSimulation $ steps) <$> getSteps steps




---- Stats mean std l2 vs nsimus ----

l2VsNSimusSteps :: Steps -> DataSet (Int, Double)
l2VsNSimusSteps (Steps (Simulation algo model _) steps') =
  DataSet $ l2VsNSimus' <$> steps'
  where l2VsNSimus' :: Step -> (Int, Double)
        l2VsNSimus' step =
          ( nSimus algo (getStepT step)
          , Model.l2 model . getStepSample $ step
          )

l2VsNSimusRun :: Run -> DataSet (Int,Double)
l2VsNSimusRun (Run algo model (RunResult _ nSteps sample)) =
  DataSet [( nSimus algo nSteps, Model.l2 model sample)]

nSimus :: Algorithm -> Int -> Int
nSimus APMC{nGen=nGen, nAlpha=nAlpha} step = nGen + nAlpha + nGen * step
nSimus MonAPMC{nGen=nGen, stepSize=stepSize, nAlpha=nAlpha} step =
  nGen + nAlpha + nGen * (step * stepSize)
nSimus SteadyState{} step = step
nSimus Beaumont2009{n=n} step = n * step

meanStdL2VsNSimus
  :: Sample (Repli Run)
  -> Either Text [(Maybe Text, DataSet (Double, Double, Double, Double))]
meanStdL2VsNSimus runs =
  -- Either Text [(group, DataSet l2VsNSimus)]
     fmap Map.toAscList
  -- Either Text (Map group (DataSet mean...))
   . (fmap . fmap) (foldl' (\acc x -> DataSet.append acc (snd x)) (DataSet []))
  -- Either Text (Map group [(nGen, DataSet [(meanNSimus, meanL2, stdNSimus, stdL2)]), ...])
   . (fmap . fmap) (sortOn fst)
  -- Either Text (Map group [(nGen, DataSet [(meanNSimus, meanL2, stdNSimus, stdL2)]), ...])
   . fmap (Map.fromListWith (<>) . Sample.get)
  -- Either Text (Sample (group, [(nGen, DataSet [(meanNSimus, meanL2, stdNSimus, stdL2)]])))
   . (fmap . Sample.map) (\((g, ng), d) -> (g, [(ng, d)]))
  -- Either Text (Sample ((group, nGen), DataSet [(meanNSimus, meanL2, stdNSimus, stdL2)]))
   . (Sample.traverse . traverse) DataSet.meanStd
  -- Sample ((group, nGen), [DataSet [(nSimus, l2)], ...])
   . Sample.map (\(Repli _ runs) ->
      ( ( group =<< headMay runs
        , nGen =<< headMay runs
        )
      , DataSet.map (first fromIntegral) . l2VsNSimusRun <$> runs
      ))
  -- Sample (Repli Run)
   $ runs
  where
    group :: Run -> Maybe Text
    group x = case x of
      (Run (APMC{pAccMin}) Model.Toy _) ->
        Just ("APMC pAccMin=" <> show pAccMin)
      (Run (MonAPMC{pAccMin}) Model.Toy _) ->
        Just ("MonAPMC pAccMin=" <> show pAccMin)
      _ -> Nothing
    nGen x = Just $ getAlgoNGen . getRunAlgo $ x



---- Stat L2 vs time K ----

-- Returns DataSet (totalTime, algoTime, simTime, L2)
l2VsTimeSteps :: Steps -> DataSet (Double, Double, Double, Double)
l2VsTimeSteps steps =
  DataSet $ stat <$> getSteps steps
  where stat step =
          ( Duration.seconds $ getStepTotalTime step
          , Duration.seconds $ getStepAlgoTime step
          , Duration.seconds $ getStepSimTime step
          , Model.l2 (getSimuModel . getStepsSimulation $ steps) . getStepSample $ step)

l2VsTimeRepliSteps :: Repli Steps -> Repli (DataSet (Double, Double, Double, Double))
l2VsTimeRepliSteps repliSteps = Repli.map l2VsTimeSteps repliSteps



---- Stat Comp LHS ----

l2Ratio :: Comp -> Double 
l2Ratio c = l2Apmc / l2MonApmc 
  where 
    l2Apmc = Model.l2 (getCompModel c) (getCompSampleApmc c)
    l2MonApmc = Model.l2 (getCompModel c) (getCompSampleMonApmc c)

timeRatio :: Comp -> Double 
timeRatio c = Duration.seconds (getCompTimeApmc c) / Duration.seconds (getCompTimeMonApmc c)

statsComp :: Comp -> (Double, Double, Double, Double, Double, Double)
statsComp c =
  let 
      l2LhsA = Model.l2 (getCompModel c) (getCompSampleApmc c)
      timeLhsA = Duration.seconds (getCompTimeApmc c)
      l2LhsM = Model.l2 (getCompModel c) (getCompSampleMonApmc c)
      timeLhsM = Duration.seconds (getCompTimeMonApmc c)
   in (l2LhsA, timeLhsA, l2LhsM, timeLhsM, l2Ratio c, timeRatio c)

statsCompLhs :: [Comp] -> [(Double, Double, Double, Double, Double, Double)]
statsCompLhs cs = fmap statsComp cs

-- TO BE REMOVED
-- Plotting

line
  :: Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> [DataSet (Double, Double)]
  -> UFig.PlotLine
line legend color style datasets =
  UFig.PlotLine legend color style
  $ fmap (\xys -> (Nothing, Just <$> DataSet.get xys)) datasets

point 
  :: Maybe Text
  -> Maybe Int
  -> Maybe Int
  -> [DataSet (Double, Double)]
  -> UFig.PlotPoint
point legend color style datasets =
   UFig.PlotPoint legend color style
  $ fmap (\xys -> (Nothing, Just <$> DataSet.get xys)) datasets

pointErrDelta
  :: (Maybe Text)
  -> (Maybe Int)
  -> (Maybe Int)
  -> [DataSet (Double, Double, Double, Double)]
  -> UFig.PlotPointErrDelta
pointErrDelta legend color style dataSets =
   UFig.PlotPointErrDelta legend color style
  $ fmap (\xys -> (Nothing, Just <$> DataSet.get xys)) dataSets

bar
  :: (Maybe Text)
  -> (Maybe Int)
  -> (Maybe Text)
  -> [DataSet (Double, Double)]
  -> UFig.PlotBar
bar legend color fillstyle dataSets =
  UFig.PlotBar legend color fillstyle
  $ fmap (\xys -> (Nothing, Just <$> DataSet.get xys)) dataSets

plotLine :: UFig.PlotLine -> UFig.Plot
plotLine = UFig.Plot . pure . UFig.PlotCmdLine

plotPoint :: UFig.PlotPoint -> UFig.Plot
plotPoint = UFig.Plot . pure . UFig.PlotCmdPoint

plotPointErrDelta :: UFig.PlotPointErrDelta -> UFig.Plot
plotPointErrDelta = UFig.Plot . pure . UFig.PlotCmdPointErrDelta

plotBar :: UFig.PlotBar -> UFig.Plot
plotBar = UFig.Plot . pure . UFig.PlotCmdBar

fig :: FilePath -> Maybe Text -> [Text] -> UFig.Plot -> UFig.Figure
fig path title prelude =
  UFig.Figure path prelude
  . UFig.SinglePlot title

rowSample :: Maybe Text -> [[Text]] -> Sample [UFig.Plot] -> UFig.PlotLayout
rowSample title plotTitles (Sample xs) =
  UFig.Multiplot UFig.Row title $ (zipWith . zipWith) (,) plotTitles xs

stackPlots :: UFig.Plot -> UFig.Plot -> UFig.Plot
stackPlots = (<>)

-- TO BE REMOVED
-- realTime :: Algorithm -> Int -> Double
-- realTime algo step = case algo of
--   -- APMC{getParallel=par} -> cpuTime step
--   MonAPMC{stepSize,parallel} ->
--    fromIntegral $ stepSize * (ceiling $ cpuTime step / fromIntegral (parallel * stepSize))
--   _ -> panic "Algorithm.realTime Not implemented."


