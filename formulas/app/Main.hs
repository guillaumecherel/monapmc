{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Protolude 

import Control.Monad.Fail (fail)
import Control.Monad.Random (Rand, RandT, MonadRandom, evalRand, evalRandT)
import Data.String (String)
import Data.Text (Text, pack, unpack, replace)
import Data.Text.IO (writeFile)
import qualified Data.Vector as V
import System.Random (StdGen, mkStdGen)
-- import Numeric
import System.FilePath (FilePath, (</>))
import System.FilePath.Glob (glob)

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import Formulas
import Input
import Output 
import qualified ABC.Lenormand2012 as Lenormand2012
import qualified ABC.SteadyState as SteadyState
import qualified Util.SteadyState as SteadyState (start, step, scanIndices) 

lenormand2012Steps :: [SimulationResult]
lenormand2012Steps = fmap getSimulationResult steps
  where steps = zip [1..] $ take 20 $ evalRand (Lenormand2012.lenormand2012 p toyModel :: Rand StdGen [Lenormand2012.S]) (mkStdGen seed)
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

steadyStateSteps :: IO [SimulationResult]
steadyStateSteps = fmap (fmap getSimulationResult) enumSteps
  where steps = flip evalRandT (mkStdGen startSeed) scan
        needSteps = [5000, 10000 .. 100000]
        enumSteps = zip needSteps <$> steps
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

saveSimulationResult :: FilePath -> SimulationResult -> IO ()
saveSimulationResult dir s = writeFile path contents
  where path = (dir </> simulationResultFileName s) 
        contents = (column (V.toList $ fmap V.head $ getSample s))

buildSimulationResult :: FilePath -> SimulationResult -> Rules ()
buildSimulationResult dir s =
  (dir </> simulationResultFileName s) %> \target -> do
    traced "simulation" (saveSimulationResult dir s)

buildLenormand2012Steps = 
  -- mconcat (fmap (buildSimulationResult "output/formulas/simulationResult/5steps") lenormand2012Steps)
  ["output/formulas/simulationResult/5steps" </> simulationResultFileName s 
    | s <- lenormand2012Steps] &%> \targets -> do
      traced "Simulation" 
    $ traverse_
      (saveSimulationResult "output/formulas/simulationResult/5steps")
      lenormand2012Steps
 
buildSteadyStateSteps :: Rules ()
buildSteadyStateSteps = do
  steadyStateSteps' <- liftIO steadyStateSteps
  liftIO $ putStrLn (show (fmap simulationResultFileName steadyStateSteps') :: String)
  ["output/formulas/simulationResult/5steps" </> simulationResultFileName s
    | s <- steadyStateSteps'] &%> \targets -> do
      traced "Simulation"
        $ traverse_
          (saveSimulationResult "output/formulas/simulationResult/5steps/")
          steadyStateSteps'
     

buildEasyABCSteps :: Rules ()
buildEasyABCSteps =
  ["output/easyABC/simulationResult/5steps/lenormand2012_5000_0.1_0.01_" <> show i <> "_1.csv" | i <- [0..4]] 
  ++ ["output/easyABC/simulationResult/5steps/beaumont2009_5000_2.00_0.01_" <> show i <> "_1.csv" | i <- [0..4]]
  &%> \targets -> do
    need ["easyABC_Steps.R"]
    cmd_ ("Rscript" :: String) ["easyABC_Steps.R" :: String]

histogramToy :: SimulationResult -> [(Double, Double)]
histogramToy s = scaledHistogram (-10) 10 300 (V.toList $ fmap V.head $ getSample s)

saveHistogram :: FilePath -> [(Double, Double)] -> IO ()
saveHistogram path histogram = writeFile path (columns2 " " histogram)

buildHistogram :: FilePath -> FilePath -> Rules ()
buildHistogram targetPattern sourceDir = do
  targetPattern %> \target -> do
    let source = sourceDir </> takeFileName target
    need [source]
    es <- liftIO $ loadSimulation source
    case es of
      Left err -> fail $ show err
      Right s -> do
        let histogram = histogramToy s
        traced "histogram" (saveHistogram target histogram)

buildHistograms :: Rules ()
buildHistograms = do
  buildHistogram 
    "output/easyABC/scaledHistogram/toy/*.csv"       
    "output/easyABC/simulationResult/5steps/"
    
  buildHistogram 
    "output/formulas/scaledHistogram/toy/*.csv"       
    "output/formulas/simulationResult/5steps/"

buildFigurePosteriorSteps :: Rules ()
buildFigurePosteriorSteps =
  "report/5steps.png" %> \target -> do
    need $ [ "report/5steps.gnuplot" ]
        ++ [ "output/formulas/scaledHistogram/toy/lenormand2012_5000_0.10_0.01_" <> show i <> "_1.csv" | i <- [1..9]]
        ++ [ "output/formulas/scaledHistogram/toy/steadyState_5000_0.10_0.01_1_" <> show (5000 * (i + 1)) <> "_1.csv" | i <- [1..9]]
        ++ [ "output/easyABC/scaledHistogram/toy/lenormand2012_5000_0.10_0.01_" <> show i <> "_1.csv" | i <- [0..4]]
        ++ [ "output/easyABC/scaledHistogram/toy/beaumont2009_5000_2.00_0.01_" <> show i <> "_1.csv" | i <- [0..4]]
    cmd_ (Cwd "report/") (unpack "gnuplot") ([unpack "-c", "5steps.gnuplot"])

main :: IO ()
main = shakeArgs shakeOptions{shakeVerbosity=Normal,
                              shakeColor=True} $ do
  want $ ["report/5steps.png"] 

  buildLenormand2012Steps
  buildSteadyStateSteps
  buildEasyABCSteps
  buildHistograms
  buildFigurePosteriorSteps

