{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module L2VSNSimus where

import Protolude 

import qualified Control.Foldl as Fold
import Control.Monad.Random.Lazy
import Data.Cached as Cached
import Data.Functor.Compose
import Data.List (last)
import Data.Text (Text, pack, unpack, unlines, intercalate)
import qualified Data.Vector as V
import Formatting
import System.Random (StdGen, mkStdGen)

import qualified Algorithm
import Figure
import Model
import Run
import Statistics
import qualified ABC.Lenormand2012 as Lenormand2012
import qualified ABC.SteadyState as SteadyState
import qualified Util.SteadyState as SteadyState 

data Algo = Lenormand2012 {getAlpha :: Double, getPAccMin :: Double}
          | SteadyState {getAlpha :: Double, getPAccMin :: Double}
  deriving (Ord, Eq, Show)

type RunC a = Compose (Rand StdGen) Cached a

run :: Algo -> Int -> RunC Run
run Lenormand2012{getAlpha=alpha, getPAccMin=pAccMin} replication = 
     Compose $ liftRandT $ fmap return (runRand (lenormand2012 alpha pAccMin replication))
run SteadyState{getAlpha=alpha, getPAccMin=pAccMin} replication = 
     Compose $ steadyState alpha pAccMin replication

lenormand2012 :: Double -> Double -> Int -> Rand StdGen (Cached Run)
lenormand2012 alpha pAccMin replication =
  let steps :: Rand StdGen [(Int, Lenormand2012.S)]
      steps = zip [1..stepMax]
          <$> Lenormand2012.scan p toyModel 
      algo = Algorithm.Lenormand2012 5000 alpha pAccMin
      p = Lenormand2012.P
        { Lenormand2012.n = Algorithm.getN algo
        , Lenormand2012.nAlpha = floor $ (Algorithm.getAlpha algo) 
                                 * (fromIntegral $ Algorithm.getN algo)
        , Lenormand2012.pAccMin = Algorithm.getPAccMin algo
        , Lenormand2012.priorSample = toyPriorRandomSample
        , Lenormand2012.priorDensity = toyPrior
        , Lenormand2012.distanceToData = rootSquaredError 0 . V.head
        }
      getRun (i, r) = Run 
        { getAlgorithm = algo
        , getStep = i
        , getReplication = replication
        , getSample = Lenormand2012.thetas r }
      cacheRun' = 
        cache' ( unpack $ "output/formulas/param_sampling/run/lenormand2012_"
                             <> sformat (fixed 2) alpha <> "_"
                             <> sformat (fixed 2) pAccMin <> "_"
                             <> show replication )
        . pure
  in do
    g <- getSplit
    return $ cacheRun' . getRun .  last $ evalRand steps g

steadyState :: Double -> Double -> Int -> Rand StdGen (Cached Run)
steadyState alpha pAccMin replication = 
  let steps :: RandT StdGen IO SteadyState.S
      steps = SteadyState.runN (stepMax * 5000) ssr
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
      getRun r = Run 
        { getAlgorithm = algo
        , getStep = SteadyState.curStep r
        , getReplication = replication
        , getSample = fmap (SteadyState.getTheta . SteadyState.getSimulation . SteadyState.getReady) (SteadyState.accepteds r) }
      cacheRun' = 
        cache' ( unpack $ "output/formulas/param_sampling/run/steadyState_"
                             <> sformat (fixed 2) alpha <> "_"
                             <> sformat (fixed 2) pAccMin <> "_"
                             <> show replication )
        . Cached.fromIO mempty
  in do
        g <- getSplit
        return $ cacheRun' $ fmap getRun $ evalRandT steps g
        
stepMax :: Int
stepMax = 100

rootSquaredError :: Double -> Double -> Double
rootSquaredError expected x = sqrt ((x - expected) ** 2)

samplePAccMin :: [Double]
-- samplePAccMin = [0.01, 0.05, 0.1, 0.2]
samplePAccMin = [0.01, 0.05]
sampleAlpha :: [Double]
-- sampleAlpha = [0.1,0.2..0.9]
sampleAlpha = [0.1, 0.2]

rep :: Algo -> RunC [Run]
rep algo = traverse (\r ->  run algo r) 
               [1..nReplications]

nReplications :: Int
nReplications = 2

l2 :: Run -> Double
l2 r = posteriorL2 (-10) 10 300 (toyPosterior 0) sample
  where sample = join $ V.toList $ V.toList <$> getSample r

nsim :: Run -> Int
nsim = nSimus

l2Mean :: Fold.Fold Run Double
l2Mean = Fold.premap l2 Fold.mean 
        
nSimMean :: Fold.Fold Run Double
nSimMean = Fold.premap (fromIntegral . nsim) Fold.mean 

data PlotData = PlotData { plotAlpha :: Double
                         , plotPAccMin :: Double
                         , plotNSimus :: Double
                         , plotL2 :: Double}
  deriving (Show)

plotData :: Algo -> RunC PlotData
plotData algo = 
  let nsimusL2 :: RunC (Double, Double)
      nsimusL2 = Fold.fold f <$> rep algo
      f = (,) <$> nSimMean <*> l2Mean
  in uncurry (PlotData (getAlpha algo) (getPAccMin algo)) <$> nsimusL2

dataLenormand2012 :: RunC [[PlotData]] 
dataLenormand2012 = (traverse . traverse) plotData algos
  where algos :: [[Algo]]
        algos = fmap (\p -> Lenormand2012 <$> sampleAlpha <*> pure p)
                     samplePAccMin

dataSteadyState :: RunC [[PlotData]] 
dataSteadyState = (traverse . traverse) plotData algos
  where algos :: [[Algo]]
        algos = fmap (\p -> SteadyState <$> sampleAlpha <*> pure p)
                     samplePAccMin

plotDataToText :: [[PlotData]] -> Text
plotDataToText datasets = 
  let rowTxt (PlotData _ _ nSim l2) = pack $ show nSim ++ " " ++ show l2
      dataSetTxt rows = unlines $ fmap rowTxt rows
  in Data.Text.intercalate "\n\n" $ map dataSetTxt datasets
        
plotL2VSNSim :: RunC ()
plotL2VSNSim =
  let lenPath = "output/formulas/l2VSNSimus/lenormand2012.csv"
      stePath = "output/formulas/l2VSNSimus/steadyState.csv"
  in  (liftC2 (<>)) (sink lenPath (pure . plotDataToText) `liftC` dataLenormand2012)
      $ (liftC2 (<>)) (sink stePath (pure . plotDataToText) `liftC` dataSteadyState)
                      (Compose $ return $ gnuplot "report/L2_vs_nsimus.png"
                                    "report/L2_vs_nsimus.gnuplot"
                                    [ ("lenormand2012", lenPath)
                                    , ("steadyState", stePath) ])

liftC :: (Cached a -> Cached b) -> RunC a -> RunC b
liftC f = Compose . liftA f . getCompose

liftC2 :: (Cached a -> Cached b -> Cached c) -> RunC a -> RunC b -> RunC c
liftC2 f a b = Compose $ liftA2 f (getCompose a) (getCompose b)

buildL2VSNSimus :: Rand StdGen (Cached ())
buildL2VSNSimus = getCompose plotL2VSNSim

