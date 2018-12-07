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

data Algo = Lenormand2012 Alpha PAccMin
          | SteadyState Alpha PAccMin
  deriving (Ord, Eq, Show)

newtype Alpha = Alpha Double deriving (Num, Eq, Show, Ord, Enum)
newtype PAccMin = PAccMin Double deriving (Num, Eq, Show, Ord, Enum)

data Replications a = Replications Int a deriving (Eq, Show)

newtype L2 a = L2 a deriving (Eq, Show, Num, Fractional, Floating)

runL2 :: Run -> L2 Double
runL2 r = L2 $ posteriorL2 (-10) 10 300 (toyPosterior 0) sample
  where sample = join $ V.toList $ V.toList <$> getSample r

newtype NSimus a = NSimus a 
  deriving (Eq, Show, Num, Enum, Ord, Fractional, Floating)

instance (Integral a) => Integral (NSimus a) where
  toInteger (NSimus a) = toInteger a
  {-# INLINE toInteger #-}
  quotRem (NSimus a) (NSimus b) = let (q,r) = quotRem a b 
                                  in (NSimus q, NSimus r)
  {-# INLINE quotRem #-}

instance (Real a) => Real (NSimus a) where
  toRational (NSimus a) = toRational a
  {-# INLINE toRational #-}

runNSim :: Run -> NSimus Int
runNSim = NSimus . nSimus

newtype Mean a = Mean a deriving (Eq, Show, Num, Fractional, Floating)
newtype Std a = Std a deriving (Eq, Show, Num, Fractional, Floating)

data Statistics = Statistics (Replications Algo) deriving (Eq, Show)

getStatistics :: Statistics 
               -> ( Mean (Replications (L2 Algo))
                  , Std (Replications (L2 Algo))
                  , Mean (Replications (NSimus Algo))
                  , Std (Replications (NSimus Algo)) )
getStatistics (Statistics (Replications n algo)) = 
  ( Mean (Replications n (L2 algo)) 
  , Std (Replications n (L2 algo))
  , Mean (Replications n (NSimus algo))
  , Std (Replications n (NSimus algo))
  )

data L2VSNSimusSeries = L2VSNSimusSeriesLenormand2012 [Alpha] PAccMin
                      | L2VSNSimusSeriesSteadyState [Alpha] PAccMin
  deriving (Eq, Show, Ord)

l2VSNSimusSeriesReplications :: Int -> L2VSNSimusSeries -> [Replications Algo]
l2VSNSimusSeriesReplications replications 
                             (L2VSNSimusSeriesLenormand2012 alphas pAccMin) =
  fmap (Replications replications . flip Lenormand2012 pAccMin) alphas
l2VSNSimusSeriesReplications replications 
                             (L2VSNSimusSeriesSteadyState alphas pAccMin) =
  fmap (Replications replications . flip SteadyState pAccMin) alphas

data Figure = Figure [L2VSNSimusSeries]
  deriving (Eq, Show, Ord)

fig = Figure 
  [ L2VSNSimusSeriesLenormand2012 alphas (PAccMin 0.01) 
  , L2VSNSimusSeriesLenormand2012 alphas (PAccMin 0.05) 
  , L2VSNSimusSeriesLenormand2012 alphas (PAccMin 0.1) 
  , L2VSNSimusSeriesLenormand2012 alphas (PAccMin 0.2) 
  , L2VSNSimusSeriesSteadyState alphas (PAccMin 0.01) 
  , L2VSNSimusSeriesSteadyState alphas (PAccMin 0.05) 
  , L2VSNSimusSeriesSteadyState alphas (PAccMin 0.1) 
  , L2VSNSimusSeriesSteadyState alphas (PAccMin 0.2) 
  ]
  where alphas = [Alpha 0.1, Alpha 0.2 .. Alpha 0.9]
  
figLenormand2012 = Figure
  [ L2VSNSimusSeriesLenormand2012 alphas (PAccMin 0.01) 
  , L2VSNSimusSeriesLenormand2012 alphas (PAccMin 0.05) 
  , L2VSNSimusSeriesLenormand2012 alphas (PAccMin 0.1) 
  , L2VSNSimusSeriesLenormand2012 alphas (PAccMin 0.2) 
  ]
  where alphas = [Alpha 0.1, Alpha 0.2 .. Alpha 0.9]

figBuilder :: FilePath -> Figure -> RunC ()
figBuilder outPath (Figure series) = 
  let replications = 10
      lenPath = "output/formulas/l2VSNSimus/lenormand2012.csv"
      stePath = "output/formulas/l2VSNSimus/steadyState.csv"
      lenSeries = filter 
        (\s -> case s of 
                 L2VSNSimusSeriesLenormand2012 _ _ -> True 
                 _ -> False)
        series
      steSeries = filter 
        (\s -> case s of 
                 L2VSNSimusSeriesSteadyState _ _ -> True 
                 _ -> False)
        series
      figData :: [L2VSNSimusSeries] -> RunC [[FigPoint]]
      figData series' = (traverse . traverse) (getFigData . Statistics)
                      $ fmap (l2VSNSimusSeriesReplications replications) 
                        series'
      csv :: FilePath -> [L2VSNSimusSeries] -> RunC ()
      csv path series' = sink path (pure . figDataSetsToText) 
        `liftC` figData series' 
      fig :: Cached ()
      fig = gnuplot outPath "report/L2_vs_nsimus.gnuplot"
                        [("lenormand2012", lenPath)
                        ,("steadyState", stePath)]
  in foldr (liftC2 (<>))  (Compose (pure fig))
                          [ csv lenPath lenSeries
                          , csv stePath steSeries ]

data FigPoint = FigPoint {fpL2Mean :: Mean (L2 Double)
                         ,fpL2Std :: Std (L2 Double)
                         ,fpNSimusMean :: Mean (NSimus Double)
                         ,fpNSimusStd :: Std (NSimus Double)}

getFigData :: Statistics -> RunC FigPoint
getFigData (Statistics (Replications n algo)) = Fold.fold f <$> runs
  where f = FigPoint <$> foldL2Mean <*> foldL2Std n
                     <*> foldNSimMean <*> foldNSimStd n
        runs :: RunC [Run]
        runs = traverse (cachedRun algo) [1..n]
                     
foldL2Mean :: Fold.Fold Run (Mean (L2 Double))
foldL2Mean = fmap Mean $ Fold.premap runL2 Fold.mean 

foldL2Std :: Int -> Fold.Fold Run (Std (L2 Double))
foldL2Std replications = fmap Std
          $ fmap (\v -> sqrt (v * fromIntegral replications / 
                                  (fromIntegral replications - 1)))
          $ Fold.premap runL2 Fold.variance

foldNSimMean :: Fold.Fold Run (Mean (NSimus Double))
foldNSimMean = Fold.premap (fromIntegral . runNSim) Fold.mean 

foldNSimStd :: Int -> Fold.Fold Run (Std (NSimus Double))
foldNSimStd replications = fmap (\v -> sqrt (v * fromIntegral replications / (fromIntegral replications - 1)))
        $ Fold.premap (fromIntegral . runNSim) Fold.variance
        
figDataSetsToText :: [[FigPoint]] -> Text
figDataSetsToText datasets = 
  let rowTxt (FigPoint (Mean (L2 l2mean)) (Std (L2 l2std))
                       (Mean (NSimus nsimusmean)) (Std (NSimus nsimusstd))) = 
        pack $ show nsimusmean ++ " " ++ 
               show l2mean ++ " " ++
               show nsimusstd ++ " " ++ 
               show l2std
      dataSetTxt rows = unlines $ fmap rowTxt rows
  in Data.Text.intercalate "\n\n" $ map dataSetTxt datasets

liftC :: (Cached a -> Cached b) -> RunC a -> RunC b
liftC f = Compose . liftA f . getCompose

liftC2 :: (Cached a -> Cached b -> Cached c) -> RunC a -> RunC b -> RunC c
liftC2 f a b = Compose $ liftA2 f (getCompose a) (getCompose b)

type RunC a = Compose (Rand StdGen) Cached a

cachedRun :: Algo -> Int -> RunC Run
cachedRun algo@(Lenormand2012 (Alpha alpha) (PAccMin pAccMin)) replication =
  Compose $ fmap c $ run algo replication 
  where c = cache' ( unpack $ 
                       "output/formulas/param_sampling/run/lenormand2012_"
                       <> sformat (fixed 2) alpha <> "_"
                       <> sformat (fixed 2) pAccMin <> "_"
                       <> show replication )
            . ( Cached.fromIO mempty )
cachedRun algo@(SteadyState (Alpha alpha) (PAccMin pAccMin)) replication =
  Compose $ fmap c $ run algo replication 
  where c = cache' ( unpack $ "output/formulas/param_sampling/run/steadyState_"
                           <> sformat (fixed 2) alpha <> "_"
                           <> sformat (fixed 2) pAccMin <> "_"
                           <> show replication )
            . ( Cached.fromIO mempty )

run :: Algo -> Int -> Rand StdGen (IO Run)
run (Lenormand2012 (Alpha alpha) (PAccMin pAccMin)) replication = 
     fmap return (lenormand2012 alpha pAccMin replication)
run (SteadyState (Alpha alpha) (PAccMin pAccMin)) replication = 
     steadyState alpha pAccMin replication

lenormand2012 :: Double -> Double -> Int -> Rand StdGen Run
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
        , Lenormand2012.distanceToData = absoluteError 0 . V.head
        }
      getRun (i, r) = Run 
        { getAlgorithm = algo
        , getStep = i
        , getReplication = replication
        , getSample = Lenormand2012.thetas r }
  in do
    g <- getSplit
    return $ getRun .  last $ evalRand steps g

steadyState :: Double -> Double -> Int -> Rand StdGen (IO Run)
steadyState alpha pAccMin replication = 
  let step :: RandT StdGen IO SteadyState.S
      step = SteadyState.runN (stepMax * 5000) ssr
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
        , SteadyState.distanceToData = absoluteError 0 . V.head
        }
      getRun r = Run 
        { getAlgorithm = algo
        , getStep = SteadyState.curStep r
        , getReplication = replication
        , getSample = fmap (SteadyState.getTheta . SteadyState.getSimulation . SteadyState.getReady) (SteadyState.accepteds r) }
  in do
        g <- getSplit
        return $ fmap getRun $ evalRandT step g
        
stepMax :: Int
stepMax = 100

rootSquaredError :: Double -> Double -> Double
rootSquaredError expected x = sqrt ((x - expected) ** 2)

absoluteError :: Double -> Double -> Double
absoluteError expected x = abs (x - expected)

buildL2VSNSimus :: Rand StdGen (Cached ())
buildL2VSNSimus = getCompose $ liftA2 (<>) 
                    (figBuilder "report/L2_vs_nsimus.png" fig) 
                    (figBuilder "report/L2_vs_nsimus_Lenormand2012.png" figLenormand2012)

