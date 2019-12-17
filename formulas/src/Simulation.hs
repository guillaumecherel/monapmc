{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Simulation where

import Protolude


import qualified ABC.Lenormand2012 as APMC
import qualified ABC.MonAPMC as MonAPMC
import qualified ABC.SteadyState as SteadyState
import qualified Control.Foldl as Fold
import           Control.Foldl (Fold)
import           Control.Monad.Random.Lazy
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Text (unpack)
import qualified Data.Vector as Vector
import           Data.Vector (Vector)
import           Formatting
import qualified Model 
import           Model (Model)
import qualified Numeric.LinearAlgebra as LA
import           Util (nestFold)
import qualified Util.Figure as UFig
import qualified Statistics
import           Text.Pretty.Simple (pShow, pShowNoColor)
import qualified Util.SteadyState as SteadyState 

data Algorithm =
  APMC
    { n :: Int
    , nAlpha :: Int
    , pAccMin :: Double
    , parallel :: Int
    }
  | MonAPMC
    { n :: Int
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
algoFilename APMC{n, nAlpha, pAccMin, parallel} =
   unpack $ "apmc_"
             <> show n <> "_"
             <> sformat (fixed 2) nAlpha <> "_"
             <> sformat (fixed 2) pAccMin <> "_"
             <> show parallel
algoFilename MonAPMC{n, nAlpha, pAccMin, stepSize, parallel, stopSampleSize} =
   unpack $ "monAPMC_"
             <> show n <> "_"
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

data Simulation = Simulation Algorithm Model Int
  -- Simulation algo model stepMax
  deriving (Show, Read)

data Steps = Steps Simulation [Step]
  -- Steps simulation steps
  deriving (Show, Read)

data Step = Step Int Double Double (Vector (Weight, Vector Double))
  -- Step t epsilon pAcc sample
  deriving (Show, Read)

getStepsSimulation :: Steps -> Simulation
getStepsSimulation (Steps x _) = x

peekSteps :: Steps -> Text
peekSteps (Steps (Simulation algo model stepMax) steps) =
  "(Steps"
  <> " algo=" <> show algo
  <> " model=" <> show model
  <> " stepMax=" <> show stepMax
  <> " steps=" <> peekList 3 peekStep steps

peekStep :: Step -> Text
peekStep (Step t epsilon pAcc sample) =
  "(Step"
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
  <> (mconcat $ intersperse ", " $ fmap peek $ take i v)
  <> if i < len then "..." else ""
  <> "]"
  where
    len = length v

getStepT :: Step -> Int
getStepT (Step t _ _ _) = t

steps :: Simulation -> RandT StdGen IO Steps
steps s@(Simulation algo model stepMax) =
  Steps s <$> (stepsResult stepMax algo model)

stepsResult :: Int -> Algorithm -> Model -> RandT StdGen IO [Step]
stepsResult stepMax APMC{n, nAlpha, pAccMin} model =
  let steps' :: Rand StdGen [APMC.S]
      steps' = take stepMax <$> APMC.scan p (Model.model model)
      p = APMC.P
        { APMC.n = n
        , APMC.nAlpha = nAlpha
        , APMC.pAccMin = pAccMin
        , APMC.priorSample = Model.priorRandomSample model
        , APMC.priorDensity = Model.prior model
        , APMC.observed = Vector.singleton 0
        }
      getStep r = Step
         (APMC.t r)
         (APMC.epsilon r)
         (APMC.pAcc r)
         (Vector.zip (Vector.fromList $ LA.toList $ APMC.weights r)
                        (Vector.fromList $ fmap Vector.fromList $ LA.toLists
                          $ APMC.thetas r))
  in getStep <<$>> mapRandT (\(Identity x) -> return x) steps'
stepsResult
  stepMax
  MonAPMC {n, nAlpha, pAccMin, stepSize, parallel , stopSampleSize}
  model =
  let steps' :: RandT StdGen IO [MonAPMC.S (RandT StdGen IO)]
      steps' = take stepMax
        <$> MonAPMC.scanPar stepSize parallel p (Model.model model)
      p = MonAPMC.P
          { MonAPMC._apmcP=APMC.P
              { APMC.n = n
              , APMC.nAlpha = nAlpha
              , APMC.pAccMin = pAccMin
              , APMC.priorSample = Model.priorRandomSample model
              , APMC.priorDensity = Model.prior model
              , APMC.observed = Vector.singleton 0
              }
          , MonAPMC._stopSampleSize=stopSampleSize
          }
      getStep MonAPMC.E = Step 0 0 0 mempty
      getStep MonAPMC.S{MonAPMC._s = s} = Step
        ( APMC.t s)
        ( APMC.epsilon s)
        ( APMC.pAcc s)
        ( Vector.zip (Vector.fromList $ LA.toList $ APMC.weights s)
              (Vector.fromList $ fmap Vector.fromList $ LA.toLists
                 $ APMC.thetas s))
  in getStep <<$>> steps'
stepsResult _ SteadyState{} _ = return mempty
stepsResult _ Beaumont2009{} _ = return mempty

data Run = Run Algorithm Model RunResult
  -- Run algo model runResult
  deriving (Show, Read)

type Weight = Double

data RunResult = RunResult Int (Vector (Weight, Vector Double))
  -- RunResult nSteps sample
  deriving (Show, Read)

run :: Simulation -> RandT StdGen IO Run
run (Simulation algo model stepMax) =
  Run algo model <$> runResult stepMax algo model

runResult :: Int -> Algorithm -> Model -> RandT StdGen IO RunResult
runResult stepMax APMC{n, nAlpha, pAccMin} model =
  let steps' :: Rand StdGen [(Int, APMC.S)]
      steps' = zip [1..stepMax] <$> APMC.scan p (Model.model model)
      p = APMC.P
        { APMC.n = n
        , APMC.nAlpha = nAlpha
        , APMC.pAccMin = pAccMin
        , APMC.priorSample = Model.priorRandomSample model
        , APMC.priorDensity = Model.prior model
        , APMC.observed = Vector.singleton 0
        }
      getRun (i, r) = RunResult i
        $ Vector.zip (Vector.fromList $ LA.toList $ APMC.weights r)
                          (Vector.fromList $ fmap Vector.fromList $ LA.toLists
                            $ APMC.thetas r)
  in getRun . List.last
    <$> mapRandT (\(Identity x) -> return x) steps'
runResult
  stepMax
  MonAPMC{n ,nAlpha ,pAccMin ,stepSize ,parallel ,stopSampleSize}
  model =
  let result :: RandT StdGen IO (MonAPMC.S (RandT StdGen IO))
      result = List.last . take stepMax
        <$> MonAPMC.scanPar stepSize parallel p (Model.model model)
      p = MonAPMC.P
          { MonAPMC._apmcP=APMC.P
              { APMC.n = n
              , APMC.nAlpha = nAlpha
              , APMC.pAccMin = pAccMin
              , APMC.priorSample = Model.priorRandomSample model
              , APMC.priorDensity = Model.prior model
              , APMC.observed = Vector.singleton 0
              }
          , MonAPMC._stopSampleSize=stopSampleSize
          }
      getRun MonAPMC.E = RunResult 0 mempty
      getRun MonAPMC.S{MonAPMC._s = s} = RunResult
        (APMC.t s)
        $ Vector.zip (Vector.fromList $ LA.toList $ APMC.weights s)
                          (Vector.fromList $ fmap Vector.fromList $ LA.toLists
                            $ APMC.thetas s)
  in getRun <$> result
runResult stepMax SteadyState{n, alpha, pAccMin, parallel} model =
  let step :: RandT StdGen IO SteadyState.S
      step = SteadyState.runN (stepMax * n) ssr
      ssr = SteadyState.runner p model'
      model' (seed, xs) = return $ evalRand (Model.model model xs) (mkStdGen seed)
      p :: SteadyState.P (RandT StdGen IO)
      p = SteadyState.P
        { SteadyState.n = n
        , SteadyState.nAlpha = floor $ alpha * (fromIntegral $ n)
        , SteadyState.pAccMin = pAccMin
        , SteadyState.parallel = parallel
        , SteadyState.priorSample = Model.priorRandomSample model
        , SteadyState.priorDensity = Model.prior model
        , SteadyState.distanceToData = Statistics.absoluteError 0 . Vector.head
        }
      getRun r = RunResult 
        (SteadyState.curStep r)
        $ fmap (\a -> (SteadyState.getWeight a, SteadyState.getTheta $ SteadyState.getSimulation $ SteadyState.getReady a)) (SteadyState.accepteds r)
  in getRun <$> step
runResult _ Beaumont2009{} _ = return (RunResult 0 mempty)


-- Data sets

data DataSet a = DataSet [a]
  deriving (Read, Show)

getDataSet :: DataSet a -> [a]
getDataSet (DataSet xs) = xs

mapDataSet :: (a -> b) -> DataSet a -> DataSet b
mapDataSet f (DataSet xs) = DataSet $ fmap f xs

sizeDataSet :: DataSet a -> Int
sizeDataSet (DataSet xs) = length xs

appendDataSets :: DataSet a -> DataSet a -> DataSet a
appendDataSets (DataSet a) (DataSet b) = DataSet $ a <> b

l2VsNSimusSteps :: Steps -> DataSet (Int, Double)
l2VsNSimusSteps (Steps (Simulation algo _ _) steps') =
  DataSet $ l2VsNSimus' <$> steps'
  where l2VsNSimus' :: Step -> (Int, Double)
        l2VsNSimus' step = (nSimus algo (getStepT step), l2ToyStep step)

l2VsNSimusRun :: Run -> DataSet (Int,Double)
l2VsNSimusRun (Run algo _ (RunResult nSteps sample)) =
  DataSet [(nSimus algo nSteps, Statistics.l2Toy sample)]

nSimus :: Algorithm -> Int -> Int
nSimus APMC{n=n, nAlpha=nAlpha} step =
  numberSimusLenormand2012 n nAlpha step
nSimus MonAPMC{n=n, stepSize=stepSize, nAlpha=nAlpha} step =
  numberSimusLenormand2012 n nAlpha (step * stepSize)
nSimus SteadyState{} step = step
nSimus Beaumont2009{n=n} step = n * step

numberSimusLenormand2012 :: Int -> Int -> Int -> Int
numberSimusLenormand2012 n nAlpha step = n + (n - nAlpha) * step

l2ToyStep :: Step -> Double
l2ToyStep (Step _ _ _ sample') = Statistics.l2Toy sample'

histogramStep :: Step -> DataSet (Double, Double)
histogramStep (Step _ _ _ sample') =
    DataSet
  $ Statistics.estPostDen (-10) 10 300
  $ fmap (second Vector.head)
  $ Vector.toList sample'

histogramSteps :: Steps -> [DataSet (Double, Double)]
histogramSteps (Steps _ steps') = histogramStep <$> steps'

meanStdDataSets
  :: [DataSet (Double, Double)] 
  -> Either Text (DataSet (Double, Double, Double, Double))
meanStdDataSets dataSets =
    DataSet . getZipList
  . Fold.fold (nestFold meanStdPoints)
  . fmap (ZipList . getDataSet)
  <$> checkLength dataSets
  where
    meanStdPoints :: Fold (Double, Double) (Double, Double, Double, Double)
    meanStdPoints =
      (,,,)
      <$> Fold.premap fst Fold.mean
      <*> Fold.premap snd Fold.mean
      <*> Fold.premap fst Fold.std
      <*> Fold.premap snd Fold.std
    checkLength :: [DataSet a] -> Either Text [DataSet a]
    checkLength [] = Right []
    checkLength (x:[]) = Right [x]
    checkLength (x:xs) = if all (sizeDataSet x ==) (sizeDataSet <$> xs)
      then Right (x:xs)
      else Left "meanStdDataSets: dataSets sizes differ. All dataSets must have the same size."

meanStdL2VsNSimus
  :: Sample (Repli Run)
  -> Either Text [(Maybe Text, DataSet (Double, Double, Double, Double))]
meanStdL2VsNSimus runs =
  -- Either Text [(group, DataSet l2VsNSimus)]
     fmap Map.toAscList
  -- Either Text (Map group (DataSet mean...))
   $ (fmap . fmap) (foldl' (\acc x -> appendDataSets acc (snd x)) (DataSet []))
  -- Either Text (Map group [(nAlpha, DataSet [(meanNSimus, meanL2, stdNSimus, stdL2)]), ...])
   $ (fmap . fmap) (sortOn fst)
  -- Either Text (Map group [(nAlpha, DataSet [(meanNSimus, meanL2, stdNSimus, stdL2)]), ...])
   $ fmap (Map.fromListWith (<>) . getSample)
  -- Either Text (Sample (group, [(nAlpha, DataSet [(meanNSimus, meanL2, stdNSimus, stdL2)]])))
   $ (fmap . mapSample) (\((g, na), d) -> (g, [(na, d)]))
  -- Either Text (Sample ((group, nAlpha), DataSet [(meanNSimus, meanL2, stdNSimus, stdL2)]))
   $ (traverseSample . traverse) meanStdDataSets
  -- Sample ((group, nAlpha), [DataSet [(nSimus, l2)], ...])
   $ mapSample (\(Repli _ runs) ->
      ( ( group =<< headMay runs
        , nAlpha =<< headMay runs
        )
      , mapDataSet (first fromIntegral) . l2VsNSimusRun <$> runs
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
    nAlpha x = case x of
      (Run (APMC n _ _ _) _ _) -> Just n
      (Run (MonAPMC n _ _ _ _ _) _ _) -> Just n
      _ -> Nothing

-- Replication functor

data Repli a = Repli Int [a]
  deriving (Show, Read)

getRepli :: Repli a -> [a]
getRepli (Repli _ xs) = xs

repli :: Int -> a -> Repli a
repli n x = Repli n $ replicate n x

repliM :: (Applicative m) => Int -> m a -> m (Repli a)
repliM n x = Repli n <$> replicateM n x

mapRepli :: (a -> b) -> Repli a -> Repli b
mapRepli f (Repli n xs) = Repli n (f <$> xs)

foldRepli :: (b -> a -> b) -> b -> Repli a -> b
foldRepli f z (Repli n xs) = List.foldl' f z xs

traverseRepli :: (Applicative f) => (a -> f b) -> Repli a -> f (Repli b)
traverseRepli f (Repli i xs) = Repli i <$> traverse f xs

aggregateRepli :: ([a] -> b) -> Repli a -> b
aggregateRepli f (Repli _ xs) = f xs

-- Sample functor

newtype Sample a = Sample [a]
  deriving (Show, Read)

getSample :: Sample a -> [a]
getSample (Sample xs) = xs

mapSample :: (a -> b) -> Sample a -> Sample b
mapSample f (Sample xs) = Sample (f <$> xs)

foldSample :: (b -> a -> b) -> b -> Sample a -> b
foldSample f z (Sample xs) = List.foldl' f z xs

traverseSample :: (Applicative f) => (a -> f b) -> Sample a -> f (Sample b)
traverseSample f (Sample xs) = Sample <$> traverse f xs

appendSample :: Sample a -> Sample a -> Sample a
appendSample (Sample as) (Sample bs) = Sample (as <> bs)

zipSample :: (a -> b -> c) -> [a] -> Sample b -> Sample c
zipSample f xs (Sample ys) = Sample $ zipWith f xs ys


-- Plotting functors

-- Plotting

line
  :: (Maybe Text)
  -> (Maybe Int)
  -> (Maybe Int)
  -> [DataSet (Double, Double)]
  -> UFig.PlotLine
line legend color style datasets =
  UFig.PlotLine legend color style
  $ fmap (\xys -> (Nothing, Just <$> getDataSet xys)) datasets

point 
  :: (Maybe Text)
  -> (Maybe Int)
  -> (Maybe Int)
  -> [DataSet (Double, Double)]
  -> UFig.PlotPoint
point legend color style datasets =
   UFig.PlotPoint legend color style
  $ fmap (\xys -> (Nothing, Just <$> getDataSet xys)) datasets

pointErrDelta
  :: (Maybe Text)
  -> (Maybe Int)
  -> (Maybe Int)
  -> [DataSet (Double, Double, Double, Double)]
  -> UFig.PlotPointErrDelta
pointErrDelta legend color style dataSets =
   UFig.PlotPointErrDelta legend color style
  $ fmap (\xys -> (Nothing, Just <$> getDataSet xys)) dataSets

bar
  :: (Maybe Text)
  -> (Maybe Int)
  -> (Maybe Text)
  -> [DataSet (Double, Double)]
  -> UFig.PlotBar
bar legend color fillstyle dataSets =
  UFig.PlotBar legend color fillstyle
  $ fmap (\xys -> (Nothing, Just <$> getDataSet xys)) dataSets

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

-- realTime :: Step -> Double
-- realTime (Step t epsilon pAcc sample) = undefined

cpuTime :: Int -> Double
cpuTime step = fromIntegral step

realTime :: Algorithm -> Int -> Double
realTime algo step = case algo of
  -- APMC{getParallel=par} -> cpuTime step
  MonAPMC{stepSize,parallel} ->
   fromIntegral $ stepSize * (ceiling $ cpuTime step / fromIntegral (parallel * stepSize))
  _ -> panic "Algorithm.realTime Not implemented."


