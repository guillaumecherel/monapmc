{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module L2VSNSimus where

import Protolude 

import qualified Control.Foldl as L
import Control.Monad.Fail (fail)
import Control.Monad.Random (Rand, RandT, MonadRandom, evalRand, evalRandT)
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

import Algorithm
import Model
import Simulation
import Statistics
import qualified ABC.Lenormand2012 as Lenormand2012
import qualified ABC.SteadyState as SteadyState
import Util.Cache
import Util.CSV
import qualified Util.SteadyState as SteadyState (start, step, scanIndices) 

lenormand2012 :: Double -> Double -> Cache SimulationResult
lenormand2012 alpha pAccMin =
  let steps = zip [1..] 
            $ evalRand (Lenormand2012.scan p toyModel 
                        :: Rand StdGen [Lenormand2012.S]) 
                       (mkStdGen seed)
      seed = 41
      algo = Lenormand2012 5000 alpha pAccMin
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
  in cacheSimulationResult "param_sampling" $ getSimulationResult $ last steps

steadyState :: Double -> Double -> IO (Cache SimulationResult)
steadyState alpha pAccMin = 
  let steps = flip evalRandT (mkStdGen startSeed) scan
      needSteps = [5000, 10000 .. 100000]
      enumSteps = zip needSteps <$> steps
      scan = SteadyState.scanIndices needSteps ssr
      ssr = SteadyState.runner p model
      model (seed, xs) = return $ evalRand (toyModel xs) (mkStdGen seed)
      startSeed = 41
      algo = SteadyState 5000 alpha pAccMin 1
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
  in fmap (cacheSimulationResult "param_sampling" . getSimulationResult . last) enumSteps

rootSquaredError expected x = sqrt ((x - expected) ** 2)

sampleAlpha = [0.1,0.2..0.9]
samplePAccMin = [0.01, 0.05, 0.1, 0.2]

l2VSNSimus :: SimulationResult -> (Double, Double)
l2VSNSimus sr = (l2, nsim)
  where l2 = posteriorL2 (-10) 10 300 (toyPosterior 0) sample
        sample = join $ V.toList $ V.toList <$> getSample sr
        nsim = nSimus sr


sr
              & L.fold (groupReplications 
                        $ (,,,) 
                        <$> pAccMinMean
                        <*> alphaMean
                        <*> posteriorL2Mean (-10) 10 300 (toyPosterior 0)
                        <*> nSimusMean)
              & Map.elems
              & L.fold ( L.groupBy (\(pAccMin, _, _, _) -> pAccMin)
                                   L.list )
              & fmap (sortBy (comparing (\(_, alpha, _,_) -> alpha)))
              & (fmap . fmap) (\(pAccMin, alpha, l2, nsimus) -> 
                                         show2dec pAccMin
                               <> " " <> show2dec alpha
                               <> " " <> show2dec l2
                               <> " " <> show2dec nsimus)
              & fmap mconcat
              & Map.elems
              & intersperse "\n\n"
              & mconcat
              
buildL2VSNSimu = do
  "output/formulas/l2_vs_nsimus/toy/lenormand2012.csv" %> \target -> do
     allSimulationResults <- liftIO $ loadAllSimulations 
       "output/formulas/simulationResult/paramSampling/"
     let simRes = filterLastStep 
                $ filterLenormand2012
                $ allSimulationResults
     let sources = 
           fmap (   ("output/formulas/simulationResult/paramSampling/" <>) 
                  . simulationResultFileName ) 
                simRes
     need sources
     let txt = simRes
              & L.fold (groupReplications 
                        $ (,,,) 
                        <$> pAccMinMean
                        <*> alphaMean
                        <*> posteriorL2Mean (-10) 10 300 (toyPosterior 0)
                        <*> nSimusMean)
              & Map.elems
              & L.fold ( L.groupBy (\(pAccMin, _, _, _) -> pAccMin)
                                   L.list )
              & fmap (sortBy (comparing (\(_, alpha, _,_) -> alpha)))
              & (fmap . fmap) (\(pAccMin, alpha, l2, nsimus) -> 
                                         show2dec pAccMin
                               <> " " <> show2dec alpha
                               <> " " <> show2dec l2
                               <> " " <> show2dec nsimus)
              & fmap mconcat
              & Map.elems
              & intersperse "\n\n"
              & mconcat
     traced "buildL2VSNSimu" $ writeFile target $ "pAccMin getAlpha l2Mean nSimusMean\n" 
                                 <> txt
    
  "output/formulas/l2_vs_nsimus/toy/steadyState.csv" %> \target -> do
     allSimulationResults <- liftIO $ loadAllSimulations 
       "output/formulas/simulationResult/paramSampling/"
     let simRes = filterLastStep 
                $ filterSteadyState
                $ allSimulationResults
     let sources = 
           fmap (   ("output/formulas/simulationResult/paramSampling/" <>) 
                  . simulationResultFileName ) 
                simRes
     need sources
     let txt = simRes
              & L.fold (groupReplications 
                        $ (,,,) 
                        <$> pAccMinMean
                        <*> alphaMean
                        <*> posteriorL2Mean (-10) 10 300 (toyPosterior 0)
                        <*> nSimusMean)
              & Map.elems
              & L.fold ( L.groupBy (\(pAccMin, _, _, _) -> pAccMin)
                                   L.list )
              & fmap (sortBy (comparing (\(_, alpha, _,_) -> alpha)))
              & (fmap . fmap) (\(pAccMin, alpha, l2, nsimus) -> 
                                         show2dec pAccMin
                               <> " " <> show2dec alpha
                               <> " " <> show2dec l2
                               <> " " <> show2dec nsimus)
              & fmap mconcat
              & Map.elems
              & intersperse "\n\n"
              & mconcat
     traced "buildL2VSNSimu" $ writeFile target $ "pAccMin getAlpha l2Mean nSimusMean\n" 
                                 <> txt
    
--             putStrLn $ columns2 " " $ L.fold (l2VsNSimus (read lowerBound) (read upperBound) (read bins) density) simRess 

buildFigureL2VSNSimu :: Rules ()
buildFigureL2VSNSimu = 
  "report/L2_vs_nsimus.png" %> \target -> do
    need $ ["report/L2_vs_nsimus.gnuplot"
           ,"output/formulas/l2_vs_nsimus/toy/lenormand2012.csv"
           ,"output/formulas/l2_vs_nsimus/toy/steadyState.csv"
           ]
    cmd_ (Cwd "report/") (unpack "gnuplot")
         ([unpack "-c", "L2_vs_nsimus.gnuplot"])

