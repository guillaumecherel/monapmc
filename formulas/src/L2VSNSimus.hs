{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module L2VSNSimus where

import Protolude 

import qualified Control.Foldl as L
import Control.Monad.Fail (fail)
import Control.Monad.Random ( Rand, RandT, MonadRandom, evalRand, evalRandT
                            , liftRandT, runRand, runRandT)
import qualified Data.Map as Map
import Data.List (last)
import Data.String (String)
import Data.Text (Text, pack, unpack, replace)
import Data.Text.IO (writeFile)
import qualified Data.Vector as V
import System.Random (StdGen, mkStdGen)
-- import Numeric
import System.FilePath (FilePath, (</>))
import System.FilePath.Glob (glob)
import Text.Printf (printf)

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import qualified Algorithm
import Model
import Run
import Statistics
import qualified ABC.Lenormand2012 as Lenormand2012
import qualified ABC.SteadyState as SteadyState
import Util.Cache
import Util.CSV
import qualified Util.SteadyState as SteadyState (start, step, scanIndices) 

data Algo = Lenormand2012 {getAlpha :: Double, getPAccMin :: Double}
          | SteadyState {getAlpha :: Double, getPAccMin :: Double}
  deriving (Ord, Eq, Show)

data RunA = RunA {unRunA :: RandT StdGen IO (Cache Run)}
   
run :: Algo -> Int -> RunA
run Lenormand2012{getAlpha=alpha, getPAccMin=pAccMin} replication = 
  RunA $ liftRandT $ fmap return (runRand (lenormand2012 alpha pAccMin replication))
run SteadyState{getAlpha=alpha, getPAccMin=pAccMin} replication = 
  RunA $ steadyState alpha pAccMin replication

lenormand2012 :: Double -> Double -> Int -> Rand StdGen (Cache Run)
lenormand2012 alpha pAccMin replication =
  let steps :: Rand StdGen [(Int, Lenormand2012.S)]
      steps = zip [1..]
          <$> Lenormand2012.scan p toyModel 
      algo = Algorithm.Lenormand2012 5000 alpha pAccMin
      p = Lenormand2012.P
        { Lenormand2012.n = Algorithm.getN algo
        , Lenormand2012.nAlpha = floor $ (Algorithm.getAlpha algo) * (fromIntegral $ Algorithm.getN algo)
        , Lenormand2012.pAccMin = Algorithm.getPAccMin algo
        , Lenormand2012.priorSample = toyPriorRandomSample
        , Lenormand2012.priorDensity = toyPrior
        , Lenormand2012.distanceToData = rootSquaredError 0 . V.head
        }
      getRun (i, s) = Run 
        { getAlgorithm = algo
        , getStep = i
        , getReplication = replication
        , getSample = Lenormand2012.thetas s }
  in fmap ( cacheRun "param_sampling"
            . getRun 
            . last ) 
          steps

steadyState :: Double -> Double -> Int -> RandT StdGen IO (Cache Run)
steadyState alpha pAccMin replication = 
  let steps :: RandT StdGen IO [SteadyState.S]
      steps = SteadyState.scanIndices needSteps ssr
      needSteps = [5000, 10000 .. 100000]
      enumSteps :: RandT StdGen IO [(Int, SteadyState.S)]
      enumSteps = zip needSteps <$> steps
      ssr = SteadyState.runner p model
      model (seed, xs) = return $ evalRand (toyModel xs) (mkStdGen seed)
      algo = Algorithm.SteadyState 5000 alpha pAccMin 1
      p :: SteadyState.P (RandT StdGen IO)
      p = SteadyState.P
        { SteadyState.n = Algorithm.getN algo
        , SteadyState.nAlpha = floor $ (Algorithm.getAlpha algo) * (fromIntegral $ Algorithm.getN algo)
        , SteadyState.pAccMin = Algorithm.getPAccMin algo
        , SteadyState.parallel = Algorithm.getParallel algo
        , SteadyState.priorSample = toyPriorRandomSample
        , SteadyState.priorDensity = toyPrior
        , SteadyState.distanceToData = rootSquaredError 0 . V.head
        }
      getRun (i, s) = Run 
        { getAlgorithm = algo
        , getStep = i
        , getReplication = replication
        , getSample = fmap (SteadyState.getTheta . SteadyState.getSimulation . SteadyState.getReady) (SteadyState.accepteds s) }
  in fmap ( cacheRun "param_sampling" 
            . getRun 
            . last ) 
          enumSteps

rootSquaredError expected x = sqrt ((x - expected) ** 2)

samplePAccMin = [0.01, 0.05, 0.1, 0.2]
sampleAlpha = [0.1,0.2..0.9]

algos = ( Lenormand2012 <$> sampleAlpha <*> samplePAccMin )
     ++ ( SteadyState <$> sampleAlpha <*> samplePAccMin )

rep :: Int -> Algo -> [RunA]
rep n algo = map (run algo) [1..n]

replications :: Map Algo [RunA]
replications = Map.fromList $ zip algos $ map (rep 2) algos

newtype RunDescriptor a = RunDescriptor (RandT StdGen IO (Uncached a))
 
l2 :: RunA -> RunDescriptor Double
l2 (RunA s) = RunDescriptor (cAp (cPure l2') <$> s)
  where l2' :: Run -> Double
        l2' sr = posteriorL2 (-10) 10 300 (toyPosterior 0) (sample sr)
        sample sr = join $ V.toList $ V.toList <$> getSample sr

nsim :: RunA -> RunDescriptor Int
nsim (RunA s) = RunDescriptor (cAp (cPure nSimus) <$> s)

-- newtype AlgoDescriptor a = AlgoDescriptor (RandT StdGen IO (Uncached a))
-- 
-- l2mean :: Algo -> Maybe (AlgoDescriptor Double)
-- l2mean algo = fmap l2 <$> (Map.lookup replications algo)

-- buildSteps :: Rules ()
-- buildSteps = foldMap buildCache (Map.values replications)
--           <> foldMap buildCache lenormand2012Steps
--           <> join (liftIO $ (fmap . foldMap) buildCache steadyStateSteps)
--           <> join (liftIO $ (fmap . foldMap) buildCache histogramsSteadyState)
--           <> buildSink figurePosteriorSteps

-- l2VSNSimus :: Run -> (Double, Double)
-- l2VSNSimus sr = (l2, nsim)
--   where l2 = posteriorL2 (-10) 10 300 (toyPosterior 0) sample
--         sample = join $ V.toList $ V.toList <$> getSample sr
--         nsim = nSimus sr
-- 
-- 
-- sr
--               & L.fold (groupReplications 
--                         $ (,,,) 
--                         <$> pAccMinMean
--                         <*> alphaMean
--                         <*> posteriorL2Mean (-10) 10 300 (toyPosterior 0)
--                         <*> nSimusMean)
--               & Map.elems
--               & L.fold ( L.groupBy (\(pAccMin, _, _, _) -> pAccMin)
--                                    L.list )
--               & fmap (sortBy (comparing (\(_, alpha, _,_) -> alpha)))
--               & (fmap . fmap) (\(pAccMin, alpha, l2, nsimus) -> 
--                                          show2dec pAccMin
--                                <> " " <> show2dec alpha
--                                <> " " <> show2dec l2
--                                <> " " <> show2dec nsimus)
--               & fmap mconcat
--               & Map.elems
--               & intersperse "\n\n"
--               & mconcat
--               
-- buildL2VSNSimu = do
--   "output/formulas/l2_vs_nsimus/toy/lenormand2012.csv" %> \target -> do
--      allRuns <- liftIO $ loadAllSimulations 
--        "output/formulas/simulationResult/paramSampling/"
--      let simRes = filterLastStep 
--                 $ filterLenormand2012
--                 $ allRuns
--      let sources = 
--            fmap (   ("output/formulas/simulationResult/paramSampling/" <>) 
--                   . simulationResultFileName ) 
--                 simRes
--      need sources
--      let txt = simRes
--               & L.fold (groupReplications 
--                         $ (,,,) 
--                         <$> pAccMinMean
--                         <*> alphaMean
--                         <*> posteriorL2Mean (-10) 10 300 (toyPosterior 0)
--                         <*> nSimusMean)
--               & Map.elems
--               & L.fold ( L.groupBy (\(pAccMin, _, _, _) -> pAccMin)
--                                    L.list )
--               & fmap (sortBy (comparing (\(_, alpha, _,_) -> alpha)))
--               & (fmap . fmap) (\(pAccMin, alpha, l2, nsimus) -> 
--                                          show2dec pAccMin
--                                <> " " <> show2dec alpha
--                                <> " " <> show2dec l2
--                                <> " " <> show2dec nsimus)
--               & fmap mconcat
--               & Map.elems
--               & intersperse "\n\n"
--               & mconcat
--      traced "buildL2VSNSimu" $ writeFile target $ "pAccMin getAlpha l2Mean nSimusMean\n" 
--                                  <> txt
--     
--   "output/formulas/l2_vs_nsimus/toy/steadyState.csv" %> \target -> do
--      allRuns <- liftIO $ loadAllSimulations 
--        "output/formulas/simulationResult/paramSampling/"
--      let simRes = filterLastStep 
--                 $ filterSteadyState
--                 $ allRuns
--      let sources = 
--            fmap (   ("output/formulas/simulationResult/paramSampling/" <>) 
--                   . simulationResultFileName ) 
--                 simRes
--      need sources
--      let txt = simRes
--               & L.fold (groupReplications 
--                         $ (,,,) 
--                         <$> pAccMinMean
--                         <*> alphaMean
--                         <*> posteriorL2Mean (-10) 10 300 (toyPosterior 0)
--                         <*> nSimusMean)
--               & Map.elems
--               & L.fold ( L.groupBy (\(pAccMin, _, _, _) -> pAccMin)
--                                    L.list )
--               & fmap (sortBy (comparing (\(_, alpha, _,_) -> alpha)))
--               & (fmap . fmap) (\(pAccMin, alpha, l2, nsimus) -> 
--                                          show2dec pAccMin
--                                <> " " <> show2dec alpha
--                                <> " " <> show2dec l2
--                                <> " " <> show2dec nsimus)
--               & fmap mconcat
--               & Map.elems
--               & intersperse "\n\n"
--               & mconcat
--      traced "buildL2VSNSimu" $ writeFile target $ "pAccMin getAlpha l2Mean nSimusMean\n" 
--                                  <> txt
--     
--             putStrLn $ columns2 " " $ L.fold (l2VsNSimus (read lowerBound) (read upperBound) (read bins) density) simRess 

-- buildFigureL2VSNSimu :: Rules ()
-- buildFigureL2VSNSimu = 
--   "report/L2_vs_nsimus.png" %> \target -> do
--     need $ ["report/L2_vs_nsimus.gnuplot"
--            ,"output/formulas/l2_vs_nsimus/toy/lenormand2012.csv"
--            ,"output/formulas/l2_vs_nsimus/toy/steadyState.csv"
--            ]
--     cmd_ (Cwd "report/") (unpack "gnuplot")
--          ([unpack "-c", "L2_vs_nsimus.gnuplot"])

