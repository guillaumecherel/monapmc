~~~~{.haskell file="formulas/src/L2VSNSimus.hs"}
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

import Formulas
import Input
import Output 
import qualified ABC.Lenormand2012 as Lenormand2012
import qualified ABC.SteadyState as SteadyState
import qualified Util.SteadyState as SteadyState (start, step, scanIndices) 
~~~~~~~~

![](report/L2_vs_nsimus.png)

Simulations need to be run for different values of pAccMin and alpha. We will keep only the final step of each simulation

~~~~ {.haskell file="formulas/src/L2VSNSimus.hs"}
lenormand2012Steps :: Double -> Double -> SimulationResult
lenormand2012Steps alpha pAccMin = getSimulationResult $ last steps
  where steps = zip [1..] $ evalRand (Lenormand2012.scan p toyModel :: Rand StdGen [Lenormand2012.S]) (mkStdGen seed)
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

steadyStateSteps :: Double -> Double -> IO SimulationResult
steadyStateSteps alpha pAccMin = fmap (getSimulationResult . last) enumSteps
  where steps = flip evalRandT (mkStdGen startSeed) scan
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
        

rootSquaredError expected x = sqrt ((x - expected) ** 2)
~~~~~~~~

Simulations are run for multiple values of alpha and pAccMin

~~~~ {.haskell file="formulas/src/L2VSNSimus.hs"}
sampleAlpha = [0.1,0.2..0.9]
samplePAccMin = [0.01, 0.05, 0.1, 0.2]
~~~~~~~~

Generate the data files for the figure.

~~~~ {.haskell file="formulas/src/L2VSNSimus.hs"}
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
~~~~~~~~

Generate the figure of L2 vs number of simulations.

~~~~ {.haskell file="formulas/src/L2VSNSimus.hs"}
buildFigureL2VSNSimu :: Rules ()
buildFigureL2VSNSimu = 
  "report/L2_vs_nsimus.png" %> \target -> do
    need $ ["report/L2_vs_nsimus.gnuplot"
           ,"output/formulas/l2_vs_nsimus/toy/lenormand2012.csv"
           ,"output/formulas/l2_vs_nsimus/toy/steadyState.csv"
           ]
    cmd_ (Cwd "report/") (unpack "gnuplot")
         ([unpack "-c", "L2_vs_nsimus.gnuplot"])
~~~~~~~~
