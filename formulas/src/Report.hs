{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}


module Report where

import Protolude 

import           Algorithm
import           Control.Monad.Random.Lazy
import           Data.Cached (Cached, sinkIO, cache', fromIO, source)
import           Data.Functor.Compose
import qualified Data.Text as Text
import qualified L2VSNSimus
import           L2VSNSimus (L2VSNSimus)
import qualified Util.Figure as Figure
import           Util.Figure (Figure(..))
import qualified Replications
import           Replications (Replications(..))
import qualified Run
import           Run (RunResult)
import qualified Steps
import           Steps (Steps(..), StepResult(..), StepsResult(..))
import           System.FilePath ((</>))
import qualified ToyModel
import           Util

cacheRootDir :: FilePath
cacheRootDir = "output/formulas/cached"

apmc :: Algorithm
apmc = Algorithm.APMC 5000 500 0.01

monAPMC :: Int -> Int -> Algorithm
monAPMC stepSize par = Algorithm.MonAPMC
  { getN = 5000
  , getNAlpha = 500
  , getPAccMin = 0.01
  , getStepSize = stepSize
  , getParallel = par
  , getStopSampleSizeFactor = 5
  }

repliAPMC :: Replications
repliAPMC = Replications
  { Replications._algorithm = apmc
  , Replications._stepMax = 100
  , Replications._nReplications = 5 }

repliMonAPMC :: Int -> Int -> Replications
repliMonAPMC stepSize par = Replications
  { Replications._algorithm = monAPMC stepSize par
  , Replications._stepMax = 100
  , Replications._nReplications = 5 }


---- Simulation results

repRun
  :: Replications -> Compose (Rand StdGen) Cached [RunResult]
repRun r@Replications
  { Replications._algorithm
  , Replications._stepMax
  , Replications._nReplications} =
  let cachePath = cacheRootDir
        </> (Text.unpack $ "replications_runs_" <> show _nReplications
             <> "_" <> show _stepMax)
        </> algoFilename _algorithm
  in liftC (cache' cachePath . fromIO mempty)
    (Compose $ Replications.repRuns r)

repSteps
  :: Replications -> Compose (Rand StdGen) Cached [StepsResult]
repSteps r@Replications
  { Replications._algorithm
  , Replications._stepMax
  , Replications._nReplications} =
  let cachePath = cacheRootDir
        </> (Text.unpack $ "replications_steps_" <> show _nReplications <> "_" <> show _stepMax)
        </> algoFilename _algorithm
  in liftC (cache' cachePath . fromIO mempty)
    (Compose $ Replications.repSteps r)

replicationsStepsAPMC :: Compose (Rand StdGen) Cached [StepsResult]
replicationsStepsAPMC = repSteps repliAPMC

replicationsStepsMonAPMC
  :: Compose (Rand StdGen) Cached [(Int, Int, [StepsResult])]
replicationsStepsMonAPMC =
  let stepSizes = [1,2]
      pars = [1,2,3,4]
  in traverse
        (\(s, p) -> (s, p,) <$> repSteps (repliMonAPMC s p))
        ((,) <$> stepSizes <*> pars)

easyABCAPMCSteps :: (Steps, Cached StepsResult)
easyABCAPMCSteps = (s,sr)
  where s = Steps.steps stepMax algo
        sr = fmap Steps.StepsResult $ traverse getStep (zip [1..] files)
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
  where s = Steps.steps stepMax algo
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


---- Figures

toyModelDistribution :: Compose (Rand StdGen) Cached ()
toyModelDistribution =
  let figPath = "report/toy_model_distribution.png"
  in 
     liftC (sinkIO figPath Figure.gnuplotInline)
   $ pure $ Figure
       figPath
       [ "set terminal png" ]
     $ Figure.SinglePlot $ Figure.Plot
         "Toy model distribution"
         [ Figure.DataLine "density" Nothing Nothing
           [ ("Density", Just <$> zip ToyModel.sample ToyModel.density)
           , ("CDF", Just <$> zip ToyModel.sample ToyModel.cdf)
           ]
         ]

stepStatVSTime
  :: FilePath
  -> Text
  -> Text
  -> (StepResult -> Double)
  -> (Algorithm -> StepResult -> Double)
  -> Compose (Rand StdGen) Cached ()
stepStatVSTime figPath title ylabel stat time =
  let 
      mkDataReplis :: Algorithm -> [StepsResult] -> [[(Double, Double)]]
      mkDataReplis algo = fmap
        $ fmap ((,) <$> time algo <*> stat)
        . Steps._steps
      mkDataMonAPMC
        :: [(Int, Int, [StepsResult])]
        -> [(Int, Int, [[(Double, Double)]])]
      mkDataMonAPMC =
          fmap (\ (stepSize, par, replis)
               -> (stepSize, par, mkDataReplis (monAPMC stepSize par) replis))
      linesAPMC :: Compose (Rand StdGen) Cached [[(Double, Double)]]
      linesAPMC = fmap (mkDataReplis apmc) replicationsStepsAPMC
      linesMonAPMC
        :: Compose (Rand StdGen) Cached [(Int, Int, [[(Double, Double)]])]
      linesMonAPMC = fmap mkDataMonAPMC replicationsStepsMonAPMC
      figInput
        :: [[(Double, Double)]]
        -> [(Int, Int, [[(Double, Double)]])]
        -> [(Text, Maybe Int, [(Text, [Maybe (Double, Double)])])]
      figInput dataAPMC dataMonAPMC =
             [( "APMC", Just 0
              ,   zip [1..] dataAPMC
                & fmap (\(i, dataset) ->
                    ("APMC replication " <> show i, Just <$> dataset)))]
          <> (  dataMonAPMC
              & fmap (\(stepSize, par, datasets) ->
                  let legend = "MonAPMC " <> show stepSize <> " " <> show par
                      linestyle = Just $ stepSize + 2
                      datasets' = zip [1..] datasets
                        & fmap (\(i, dataset) ->
                            ( legend <> " replication " <> show i
                            , Just <$> dataset))
                  in (legend, linestyle, datasets')))
      gnuplotPrelude =
        [ "set terminal png truecolor font ',10'" :: Text
        , "set grid"
        , "set xlabel 'time'"
        , "set yrange [0:*]"
        , "set xtics 1"
        ]
        <> ["set ylabel '" <> ylabel <> "'"]
      fig 
        :: [[(Double, Double)]]
        -> [(Int, Int, [[(Double, Double)]])]
        -> IO ()
      fig a b = Figure.gnuplotInline
            $ Figure figPath gnuplotPrelude
            $ Figure.SinglePlot $ Figure.Plot title
            $ (\(t,c,d) -> Figure.DataLine t c c d) <$> figInput a b
  in liftC (sinkIO figPath identity)
   $ liftA2 fig
     linesAPMC
     linesMonAPMC

epsilonVSCPUTime :: Compose (Rand StdGen) Cached ()
epsilonVSCPUTime =
  stepStatVSTime
    "report/epsilon_vs_cputime.png"
    "Epsilon VS CPU time"
    "epsilon"
     Steps._epsilon
     (\_ -> cpuTime . Steps._t)

epsilonVSRealTime :: Compose (Rand StdGen) Cached ()
epsilonVSRealTime =
  stepStatVSTime
    "report/epsilon_vs_real.png"
    "Epsilon VS real time"
    "epsilon"
     Steps._epsilon
     (\algo -> realTime algo . Steps._t)

l2VSCPUTime :: Compose (Rand StdGen) Cached ()
l2VSCPUTime =
  stepStatVSTime
    "report/l2_vs_cputime.png"
    "L2 VS CPU time"
    "l2"
     Steps.l2Toy
     (\_ -> cpuTime . Steps._t)

l2VSRealTime :: Compose (Rand StdGen) Cached ()
l2VSRealTime =
  stepStatVSTime
    "report/l2_vs_real.png"
    "L2 VS real time"
    "L2"
     Steps.l2Toy
     (\algo -> realTime algo . Steps._t)

pAccVSCPUTime :: Compose (Rand StdGen) Cached ()
pAccVSCPUTime =
  stepStatVSTime
    "report/pAcc_vs_cputime.png"
    "pAcc VS CPU time"
    "pAcc"
     Steps._pAcc
     (\_ -> cpuTime . Steps._t)

pAccVSRealTime :: Compose (Rand StdGen) Cached ()
pAccVSRealTime =
  stepStatVSTime
    "report/pAcc_vs_real.png"
    "pAcc VS real time"
    "pAcc"
     Steps._pAcc
     (\algo -> realTime algo . Steps._t)




histogramSteps
  :: Algorithm
  -> Compose (Rand StdGen) Cached [(Int, [(Double, Double)])]
histogramSteps algo =
  let stepMax = 100
      histogramStepsCachePath =
        cacheRootDir </> "scaledHistogram/toy" </> algoFilename algo
      cachedStepsPath =
        cacheRootDir </> "steps_" <> show stepMax
                     </> algoFilename algo
  in  liftC (cache' (histogramStepsCachePath))
    $ fmap (fmap ((,) <$> _t <*> Steps.histogramStep) . _steps)
    $ liftC ( cache' (cachedStepsPath)
            . fromIO mempty )
    $ Compose $ Steps.stepsResult (Steps.steps stepMax algo)

histogramsEasyABCAPMC :: Cached [(Int, [(Double, Double)])]
histogramsEasyABCAPMC = 
  snd easyABCAPMCSteps
  & fmap _steps
  & (fmap . fmap) ((,) <$> _t <*> Steps.histogramStep)
  & cache' "output/easyABC/scaledHistogram/toy/lenormand2012" 

histogramsEasyABCBeaumont2009 :: Cached [(Int, [(Double, Double)])]
histogramsEasyABCBeaumont2009 = 
  snd easyABCBeaumont2009Steps
  & fmap _steps
  & (fmap . fmap) ((,) <$> _t <*> Steps.histogramStep)
  & cache' "output/easyABC/scaledHistogram/toy/beaumont2009"





steps :: Compose (Rand StdGen) Cached ()
steps =
  let figPath = "report/steps.png"
      normal x mean var = exp (- ((x - mean) ** 2.0) / (2.0 * var)) / (sqrt (2.0 * pi * var))
      posterior x = 0.5 * normal x 0.0 (1.0 / 100.0) + 0.5 * normal x 0.0 1.0
      figData :: Compose (Rand StdGen) Cached [(Text, [(Int, [(Double, Double)])])]
      figData = liftA2 (<>)
                    (pure <$> ("APMC",) <$> histogramSteps apmc)
                $ liftA2 (<>)
                    (pure <$> ("MonAPMC StepSize 1 Parallel2",) <$>
                      histogramSteps (monAPMC 1 2))
                $ liftA2 (<>)
                    (pure <$> ("MonAPMC StepSize 2 Parallel1",) <$>
                      histogramSteps (monAPMC 2 1))
                $ liftA2 (<>)
                    (pure <$> ("EasyABC APMC",)
                          <$> Compose (pure histogramsEasyABCAPMC))
                    (pure <$> ("EasyABC Beaumont2009",)
                          <$> Compose (pure histogramsEasyABCBeaumont2009))
      fig :: [(Text, [(Int, [(Double, Double)])])] -> Figure
      fig x =
        let rows = length x
            columns = maximum $ fmap (length . snd) x
            figWidth = fromIntegral columns * 1.4 * 72 * 2.54
            figHeight = fromIntegral rows * 1.4 * 72 * 2.54
            prelude =
              [ "set terminal png truecolor size "
                <> show figWidth <> "," <> show figHeight <> " font ',12'"
              , "set output '" <> Text.pack figPath <> "'"
              , "set xrange [-4:4]"
              , "set yrange [0:3]"
              , "set samples 500"
              , "set isosamples 500"
              , "set style fill solid 1.0\n"
              ]
        in Figure figPath prelude
          $ Figure.Multiplot Figure.Row "Steps"
            $ x & fmap (\(algoLabel, x')
               -> x' & fmap (\(step, hist)
                 -> Figure.Plot (algoLabel <> " " <> show step)
                   $ [Figure.DataBar "" Nothing Nothing
                     [(algoLabel <> " " <> show step, hist)]]))
  in liftC (sinkIO figPath (Figure.gnuplotInline . fig)) figData


l2VsNSimus :: Compose (Rand StdGen) Cached ()
l2VsNSimus =
  let figPath = "report/L2_vs_nsimus.png"
      nReplications = 10
      nAlpha = 500
      --alphas = [1%10, 2%10 .. 9%10]
      --ns = fmap (\a -> floor (fromIntegral nAlpha/ a) alphas
      ns = [501, 555, 625, 1000, 5000]
      pAccMins = [0.01, 0.05, 0.1, 0.2]
      stepMax = 100
      prelude =
        [ "set terminal png truecolor" :: Text
        , "set output '" <> Text.pack figPath <> "'"
        , "set grid"
        , "set key on inside horizontal"
        , "# set yrange [0:.25]"
        , "set ytics 0,.05"
        , "# set xrange [0.2e5:33e5]"
        , "set logscale x 2"
        , "# set xtics (0.125e5, 0.25e5, 0.5e5, 1e5, 2e5, 4e5, 8e5, 16e5, 32e5)"
        , "set xtics 3125, 2"
        ]
      len n nAlpha pAccMin = APMC
                            { getN = n
                            , getNAlpha = nAlpha
                            , getPAccMin=pAccMin}
      moa stepSize parallel n nAlpha pAccMin = MonAPMC
                            { getN = n
                            , getNAlpha = nAlpha
                            , getPAccMin = pAccMin
                            , getStepSize = stepSize
                            , getParallel = parallel
                            , getStopSampleSizeFactor = 5}
      algos :: [(Text, [(Double, [(Int, Algorithm)])])]
      algos =
        [("APMC", len), ("MonAPMC", moa 1 1)] &
          fmap (\(label, algo) ->
            (label, pAccMins & fmap (\pAccMin ->
              (pAccMin, ns & fmap (\n ->
                (n, algo n nAlpha pAccMin))))))
      l2VsNSimus :: Algorithm -> [RunResult] -> L2VSNSimus
      l2VsNSimus algo runResults =
        L2VSNSimus.l2VSNSimus algo runResults
      figData
        :: [(Text, [(Double, [(Int, Algorithm)])])]
        -> Compose (Rand StdGen) Cached
          [(Text, [(Double, [(Int, L2VSNSimus)])])]
      figData algos =
        (traverse . traverse . traverse . traverse . traverse . traverse)
          (\algo -> l2VsNSimus algo <<$>> repRun
            $ Replications algo stepMax nReplications)
          algos
      fig :: [(Text, [(Double, [(Int, L2VSNSimus)])])] -> Figure
      fig x =
        Figure figPath prelude
          $ Figure.SinglePlot $ Figure.Plot "L2 vs nSimus"
            $ mconcat
              $ zip [1..] x & fmap (\(i, (algoLabel, x'))
                -> zip [1..] x' & fmap (\(j, (pAccMin, x''))
                  -> Figure.DataPointErrDelta
                    (algoLabel <> " " <> show pAccMin)
                    (Just i)
                    (Just j)
                    $ x'' & fmap (\(n, lvns)
                      ->
                        ( "algo=" <> algoLabel <> " " <>
                          "n=" <> show n <> " " <>
                          "pAccMin=" <> show pAccMin
                        , [ ( L2VSNSimus._nSimusMean lvns
                            , L2VSNSimus._l2Mean lvns
                            , L2VSNSimus._nSimusStd lvns
                            , L2VSNSimus._l2Std lvns
                            )
                          ]
                        ))))
  in liftC (sinkIO figPath (Figure.gnuplotInline . fig)) (figData algos)


report :: Compose (Rand StdGen) Cached ()
report = foldl' (liftA2 (<>)) (pure ())
 [ toyModelDistribution
 , epsilonVSCPUTime
 , epsilonVSRealTime
 , l2VSCPUTime
 , l2VSRealTime
 , pAccVSCPUTime
 , pAccVSRealTime
 , steps
 , l2VsNSimus
 ]


