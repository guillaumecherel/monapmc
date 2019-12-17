{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}


module Report where

import Protolude 

import           Control.Monad.Random.Lazy
import           Control.Monad.Except
import qualified Data.Cached as Cached
import           Data.Cached (Cached)
import           Data.Functor.Compose
import qualified Data.Text as Text
import           Simulation
import           System.FilePath ((</>))
import qualified ToyModel
import qualified Model
import           Model (Model(..))
import           Util
import qualified Util.Figure as Figure
import           Util.Figure (Figure)
 
cacheRootDir :: FilePath
cacheRootDir = "output/formulas/cached"
-- 
-- -- Cached  Category: morphisms
-- cacheMorphism :: (Show b, Read b) => FilePath -> (a -> b) -> Cache a -> Cached  b
-- cacheMorphism path f = cache' path . fmap f
--
-- -- Cached  Functor: map Objects
-- mapCache :: (Show a, Read a) => FilePath -> a -> Cached  a
-- mapCache path = cache' path . pure
-- 
-- mapCacheRepli :: (Show a, Read a) => FilePath -> Repli a -> Cached  (Repli a)
-- mapCacheRepli path ra =
-- 
-- mapCacheSample :: (Show a, Read a) => FilePath -> Sample a -> Cached  (Sample a)
-- mapCacheSample = mapCache
-- 
-- -- Cached  Functor: map morphisms
-- mapCacheMorphism
--   :: (Show b, Read b)
--   => FilePath
--   -> (a -> b) 
--   -> Cached  a
--   -> Cached  b
-- mapCacheMorphism = cacheMorphism
--   
-- mapCacheMorphismRepli
--   :: (Show a, Read a)
--   => FilePath
--   -> (Repli a -> Repli b)
--   -> Cached  (Repli a)
--   -> Cached  (Repli b)
-- 
-- mapCacheMorphismSample
--   :: (Show a, Read a)
--   => FilePath
--   -> (Sample a -> Sample b)
--   -> Cached  (Sample a)
--   -> Cached  (Sample b)

apmcDef :: Algorithm
apmcDef = APMC 5000 500 0.01 1

monAPMCDef :: Int -> Int -> Algorithm
monAPMCDef stepSize par = MonAPMC
  { n = 5000
  , nAlpha = 500
  , pAccMin = 0.01
  , stepSize = stepSize
  , parallel = par
  , stopSampleSize = 5000
  }
-- 
-- repliAPMC :: Replications
-- repliAPMC = Replications
--   { Replications._algorithm = apmc
--   , Replications._stepMax = 100
--   , Replications._nReplications = 5 }
-- 
-- repliMonAPMC :: Int -> Int -> Replications
-- repliMonAPMC stepSize par = Replications
--   { Replications._algorithm = monAPMC stepSize par
--   , Replications._stepMax = 100
--   , Replications._nReplications = 5 }


---- Simulation results

-- repRun
--   :: Replications -> Compose (Rand StdGen) Cached [RunResult]
-- repRun r@Replications
--   { Replications._algorithm
--   , Replications._stepMax
--   , Replications._nReplications} =
--   let cachePath = cacheRootDir
--         </> (Text.unpack $ "replications_runs_" <> show _nReplications
--              <> "_" <> show _stepMax)
--         </> algoFilename _algorithm
--   in liftC (cache' cachePath . fromIO mempty)
--     (Compose $ Replications.repRuns r)
-- 
-- repSteps
--   :: Replications -> Compose (Rand StdGen) Cached [StepsResult]
-- repSteps r@Replications
--   { Replications._algorithm
--   , Replications._stepMax
--   , Replications._nReplications} =
--   let cachePath = cacheRootDir
--         </> (Text.unpack $ "replications_steps_" <> show _nReplications <> "_" <> show _stepMax)
--         </> algoFilename _algorithm
--   in liftC (cache' cachePath . fromIO mempty)
--     (Compose $ Replications.repSteps r)
-- 
-- replicationsStepsAPMC :: Compose (Rand StdGen) Cached [StepsResult]
-- replicationsStepsAPMC = repSteps repliAPMC
-- 
-- replicationsStepsMonAPMC
--   :: Compose (Rand StdGen) Cached [(Int, Int, [StepsResult])]
-- replicationsStepsMonAPMC =
--   let stepSizes = [1,2]
--       pars = [1,2,3,4]
--   in traverse
--         (\(s, p) -> (s, p,) <$> repSteps (repliMonAPMC s p))
--         ((,) <$> stepSizes <*> pars)
--

type X = Compose (RandT StdGen IO) (Compose Cached (Either Text))

easyAbcApmcSteps :: Cached Steps
easyAbcApmcSteps = Steps (Simulation algo model stepMax) <$> steps
  where
    steps :: Cached  [Step]
    steps = traverse getStep (zip [1..] files)
    getStep :: (Int, FilePath) -> Cached  Step
    getStep (i,f) = Cached.source f $ read i f
    algo = APMC 5000 500 0.01 1
    model = Model.Toy
    stepMax = 0
    read :: Int -> FilePath -> Text -> Either Text Step
    read i f = bimap show (Step i 0 0) . Util.read1DSample f
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

easyAbcBeaumont2009Steps :: Cached Steps
easyAbcBeaumont2009Steps = Steps (Simulation algo model stepMax) <$> steps
  where
    steps = traverse getStep (zip [1..] files)
    getStep :: (Int, FilePath) -> Cached  Step
    getStep (i,f) = Cached.source f $ read i f
    algo = Beaumont2009 5000 2.0 0.01
    model = Model.Toy
    stepMax = 0
    read :: Int -> FilePath -> Text -> Either Text Step
    read i f = bimap show (Step i 0 0) . Util.read1DSample f
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
 
toyModelDistribution :: Figure
toyModelDistribution =
  Figure.Figure
       "report/toy_model_distribution.png"
       [ "set terminal png" ]
     $ Figure.SinglePlot (Just "Toy model distribution")
       $ Figure.Plot
         [ Figure.PlotCmdLine $ Figure.PlotLine
           (Just "density") Nothing Nothing
           [ ( Just "Density"
             , Just <$> zip ToyModel.sample ToyModel.density)
           , ( Just "CDF"
             , Just <$> zip ToyModel.sample ToyModel.cdf)
           ]
         ]
 
-- steps :: X Figure
-- steps =
--   fmap (Figure.Figure "report/steps.png"
--     [ "set terminal png truecolor size "
--       <> show figWidth <> "," <> show figHeight <> " font ',12'"
--     , "set xrange [-4:4]"
--     , "set yrange [0:3]"
--     , "set samples 500"
--     , "set isosamples 500"
--     , "set style fill solid 1.0\n"
--     , "set key off"
--     ])
--   $ fmap (rowSample Nothing titles)
--   $ ( fmap . mapSample . fmap )
--     ( flip stackPlots theoreticalLinePlot
--       . plotBar . bar Nothing (Just 3) Nothing . pure)
--   $ (fmap . mapSample) histogramSteps
--   $ (liftA2 $ flip appendSample) easyABCSteps 
--   $ (liftC . liftCIn) (Cached.cache' (cacheRootDir </> "simulation/steps"))
--   $ Compose . fmap pure
--   $ traverseSample Simulation.steps
--   $ mapSample (\algo -> Simulation algo Model.Toy 100)
--   $ Sample [apmcDef, monAPMCDef 1 2, monAPMCDef 2 1]
--   where
--     easyABCSteps :: X (Sample Steps)
--     easyABCSteps =
--       Compose . pure . Compose . pure . fmap Sample
--       $ sequenceA [easyAbcApmcSteps, easyAbcBeaumont2009Steps]
--     theoreticalLinePlot :: Figure.Plot
--     theoreticalLinePlot =
--       plotLine
--       $ line Nothing (Just 0) (Just 1)
--       $ pure
--       $ DataSet
--       $ fmap ((,) <$> identity <*> posterior) [-4, -3.99 .. 4]
--     posterior x = 0.5 * normal x 0.0 (1.0 / 100.0) + 0.5 * normal x 0.0 1.0
--     normal x mean var =
--       exp (- ((x - mean) ** 2.0) / (2.0 * var)) / (sqrt (2.0 * pi * var))
--     titles =
--       [ ("APMC\\nStep " <>) . show <$> [1..]
--       , ("MonAPMC\\nStepSize 1 Parallel 2\\nStep " <>) . show <$> [1..]
--       , ("MonAPMC\\nStepSize 2 Parallel 1\\nStep " <>) . show <$> [1..]
--       , ("EasyABC APMC\\nStep " <>) . show <$> [1..]
--       , ("EasyABC Beaumont2009\\nStep " <>) . show <$> [1..]
--       ]
--     figWidth = 10 * 1.4 * 72 * 2.54 :: Double
--     figHeight = 5 * 1.4 * 72 * 2.54 :: Double



-- l2VsNSimus :: X Figure
-- l2VsNSimus =
--   fmap (Figure.Figure "report/L2_vs_nSimus.png"
--     [ "set terminal png truecolor" :: Text
--     , "set grid"
--     , "set key on inside horizontal"
--     , "# set yrange [0:.25]"
--     , "set ytics 0,.05"
--     , "# set xrange [0.2e5:33e5]"
--     , "set logscale x 2"
--     , "# set xtics (0.125e5, 0.25e5, 0.5e5, 1e5, 2e5, 4e5, 8e5, 16e5, 32e5)"
--     , "set xtics 3125, 2"
--     ])
--   $ ((fmap . traverseSample) (meanStdDataSets . getRepli)
--      :: ())
--   $ (fmap . mapSample . mapRepli)
--     ((mapDataSet . first) fromIntegral Simulation.l2VsNSimus)
--   $ liftC (Cached.cache' (cacheRootDir </> "simulation/l2VsNSimus"))
--   $ Compose . fmap pure
--   $ (traverseSample . traverseRepli) run
--   $ mapSample (\algo -> repli 10 $ Simulation algo Model.Toy 100)
--   $ Sample
--     ( [ \n nAlpha pAccMin -> APMC n nAlpha pAccMin par
--       , \n nAlpha pAccMin -> MonAPMC n nAlpha pAccMin 1 1 n
--       ]
--       <$> [501, 555, 625, 1000, 5000]
--       <*> [500]
--       <*> [0.01, 0.05, 0.1, 0.2]
--     )



-- l2VsNSimus :: Compose (Rand StdGen) Cached ()
-- l2VsNSimus =
--   let figPath = "report/L2_vs_nsimus.png"
--       nReplications = 10
--       nAlpha = 500
--       --alphas = [1%10, 2%10 .. 9%10]
--       --ns = fmap (\a -> floor (fromIntegral nAlpha/ a) alphas
--       ns = [501, 555, 625, 1000, 5000]
--       pAccMins = [0.01, 0.05, 0.1, 0.2]
--       stepMax = 100
--       prelude =
--         [ "set terminal png truecolor" :: Text
--         , "set output '" <> Text.pack figPath <> "'"
--         , "set grid"
--         , "set key on inside horizontal"
--         , "# set yrange [0:.25]"
--         , "set ytics 0,.05"
--         , "# set xrange [0.2e5:33e5]"
--         , "set logscale x 2"
--         , "# set xtics (0.125e5, 0.25e5, 0.5e5, 1e5, 2e5, 4e5, 8e5, 16e5, 32e5)"
--         , "set xtics 3125, 2"
--         ]
--       len n nAlpha' pAccMin = APMC
--                             { getN = n
--                             , getNAlpha = nAlpha'
--                             , getPAccMin=pAccMin
--                             , getParallel=1}
--       moa stepSize parallel n nAlpha' pAccMin = MonAPMC
--                             { getN = n
--                             , getNAlpha = nAlpha'
--                             , getPAccMin = pAccMin
--                             , getStepSize = stepSize
--                             , getParallel = parallel
--                             , getStopSampleSize = 5000}
--       algos :: [(Text, [(Double, [(Int, Algorithm)])])]
--       algos =
--         [("APMC", len), ("MonAPMC", moa 1 1)] &
--           fmap (\(label, algo) ->
--             (label, pAccMins & fmap (\pAccMin ->
--               (pAccMin, ns & fmap (\n ->
--                 (n, algo n nAlpha pAccMin))))))
--       figData
--         :: Compose (Rand StdGen) Cached
--           [(Text, [(Double, [(Int, L2VSNSimus)])])]
--       figData  =
--         (traverse . traverse . traverse . traverse . traverse . traverse)
--           (\algo -> L2VSNSimus.l2VSNSimus algo <<$>> repRun
--             $ Replications algo stepMax nReplications)
--           algos
--       fig :: [(Text, [(Double, [(Int, L2VSNSimus)])])] -> Figure
--       fig x =
--         Figure figPath prelude
--           $ Figure.SinglePlot $ Figure.Plot "L2 vs nSimus"
--             $ mconcat
--               $ zip [1..] x & fmap (\(i, (algoLabel, x'))
--                 -> zip [1..] x' & fmap (\(j, (pAccMin, x''))
--                   -> Figure.DataPointErrDelta
--                     (algoLabel <> " " <> show pAccMin)
--                     (Just i)
--                     (Just j)
--                     $ x'' & fmap (\(n, lvns)
--                       ->
--                         ( "algo=" <> algoLabel <> " " <>
--                           "n=" <> show n <> " " <>
--                           "pAccMin=" <> show pAccMin
--                         , [ ( L2VSNSimus._nSimusMean lvns
--                             , L2VSNSimus._l2Mean lvns
--                             , L2VSNSimus._nSimusStd lvns
--                             , L2VSNSimus._l2Std lvns
--                             )
--                           ]
--                         ))))
--   in Util.makeFigure figPath (fig <$> figData)
-- 
-- 
-- 
-- 
-- 
-- 
-- stepStatVSTime
--   :: FilePath
--   -> Text
--   -> Text
--   -> (StepResult -> Double)
--   -> (Algorithm -> StepResult -> Double)
--   -> Compose (Rand StdGen) Cached ()
-- stepStatVSTime figPath title ylabel stat time =
--   let 
--       mkDataReplis :: Algorithm -> [StepsResult] -> [[(Double, Double)]]
--       mkDataReplis algo = fmap
--         $ fmap ((,) <$> time algo <*> stat)
--         . Steps._steps
--       mkDataMonAPMC
--         :: [(Int, Int, [StepsResult])]
--         -> [(Int, Int, [[(Double, Double)]])]
--       mkDataMonAPMC =
--           fmap (\ (stepSize, par, replis)
--                -> (stepSize, par, mkDataReplis (monAPMC stepSize par) replis))
--       linesAPMC :: Compose (Rand StdGen) Cached [[(Double, Double)]]
--       linesAPMC = fmap (mkDataReplis apmc) replicationsStepsAPMC
--       linesMonAPMC
--         :: Compose (Rand StdGen) Cached [(Int, Int, [[(Double, Double)]])]
--       linesMonAPMC = fmap mkDataMonAPMC replicationsStepsMonAPMC
--       figInput
--         :: [[(Double, Double)]]
--         -> [(Int, Int, [[(Double, Double)]])]
--         -> [(Text, Maybe Int, [(Text, [Maybe (Double, Double)])])]
--       figInput dataAPMC dataMonAPMC =
--              [( "APMC", Just 0
--               ,   zip [1 :: Int  ..] dataAPMC
--                 & fmap (\(i, dataset) ->
--                     ("APMC replication " <> show i, Just <$> dataset)))]
--           <> (  dataMonAPMC
--               & fmap (\(stepSize, par, datasets) ->
--                   let legend = "MonAPMC " <> show stepSize <> " " <> show par
--                       linestyle = Just $ stepSize + 2
--                       datasets' = zip [1 :: Int ..] datasets
--                         & fmap (\(i, dataset) ->
--                             ( legend <> " replication " <> show i
--                             , Just <$> dataset))
--                   in (legend, linestyle, datasets')))
--       gnuplotPrelude =
--         [ "set terminal png truecolor font ',10'" :: Text
--         , "set grid"
--         , "set xlabel 'time'"
--         , "set yrange [0:*]"
--         , "set xtics 1"
--         ]
--         <> ["set ylabel '" <> ylabel <> "'"]
--       fig 
--         :: [[(Double, Double)]]
--         -> [(Int, Int, [[(Double, Double)]])]
--         -> Figure
--       fig a b =
--           Figure figPath gnuplotPrelude
--             $ Figure.SinglePlot $ Figure.Plot title
--             $ (\(t,c,d) -> Figure.DataLine t c c d) <$> figInput a b
--   in Util.makeFigure figPath (fig <$> linesAPMC <*> linesMonAPMC)
-- 
-- epsilonVSCPUTime :: Compose (Rand StdGen) Cached ()
-- epsilonVSCPUTime =
--   stepStatVSTime
--     "report/epsilon_vs_cputime.png"
--     "Epsilon VS CPU time"
--     "epsilon"
--      Steps._epsilon
--      (\_ -> cpuTime . Steps._t)
-- 
-- epsilonVSRealTime :: Compose (Rand StdGen) Cached ()
-- epsilonVSRealTime =
--   stepStatVSTime
--     "report/epsilon_vs_real.png"
--     "Epsilon VS real time"
--     "epsilon"
--      Steps._epsilon
--      (\algo -> realTime algo . Steps._t)
-- 
-- l2VSCPUTime :: Compose (Rand StdGen) Cached ()
-- l2VSCPUTime =
--   stepStatVSTime
--     "report/l2_vs_cputime.png"
--     "L2 VS CPU time"
--     "l2"
--      Steps.l2Toy
--      (\_ -> cpuTime . Steps._t)
-- 
-- -- l2VSRealTime :: Compose (Rand StdGen) Cached ()
-- -- l2VSRealTime =
-- --   stepStatVSTime
-- --     "report/l2_vs_real.png"
-- --     "L2 VS real time"
-- --     "L2"
-- --      Steps.l2Toy
-- --      (\algo -> realTime algo . Steps._t)
-- 
-- pAccVSCPUTime :: Compose (Rand StdGen) Cached ()
-- pAccVSCPUTime =
--   stepStatVSTime
--     "report/pAcc_vs_cputime.png"
--     "pAcc VS CPU time"
--     "pAcc"
--      Steps._pAcc
--      (\_ -> cpuTime . Steps._t)
-- 
-- pAccVSRealTime :: Compose (Rand StdGen) Cached ()
-- pAccVSRealTime =
--   stepStatVSTime
--     "report/pAcc_vs_real.png"
--     "pAcc VS real time"
--     "pAcc"
--      Steps._pAcc
--      (\algo -> realTime algo . Steps._t)
-- 
-- -- l2VsTime :: Compose (Rand StdGen) Cached ()
-- -- l2VsTime =
-- --   let parallel = [1,2,4]
-- --       replications = 2
-- --       grid =
-- --         [ [ ("APMC", 5000, 500, 0.1, apmc)
-- --           , ("APMC", 5000, 500, 0.01, apmc)
-- --           ]
-- --         , [ ("MonAPMC", 5000, 500, 0.1, monApmc)
-- --           , ("MonAPMC", 5000, 500, 0.01, monApmc)
-- --           ]
-- --         ]
-- --       apmc n nAlpha pAccMin parallel = Algorithm.APMC n nAlpha pAccMin par
-- --       monApmc n nAlpha pAccMin parallel =
-- --         let stopSampleSize = n
-- --         in Algorithm.MonAPMC n nAlpha pAccMin 1 par stopSampleSize
-- --       title (name, n, nAlpha, pAccMin, _) =
-- --         name <> "n=" <> show n <> " nAlpha=" <> show nAlpha
-- --           <> " pAccMin=" <> show pAccMi
-- --       algo (_, n, nAlpha, pAccMin, algoConst) par =
-- --         algoConst n nAlpha pAccMin par
-- --       algoPars specs = fmap (\p -> (p, algo specs p)) parallel
-- --       replicateBench :: Int -> Benchmark -> Rand StdGen [Benchmark.Result]
-- --       replicateBench n b = replicateM n $ Benchmark.run b
-- --       cacheBen
-- --         :: Benchmark
-- --         -> Benchmark.Result
-- --         -> Cached Benchmark.Result
-- --       cacheBen b r = cache' (cacheRootDir </> Benchmark.filename b) (pure r)
-- --       plot :: Text -> [(Int, [[(Double, Double)]])] -> Plot
-- --       plot title lines  =
-- --         Plot title
-- --           (lines & fmap (\(par, lines')
-- --             -> DataLine
-- --               ("par=" <> show par)
-- --               Nothing
-- --               (Just 1)
-- --               (zipWith
-- --                 (\i line
-- --                   -> ("Replication " <> show i, fmap Just line))
-- --                 [1..]
-- --                 lines')))
-- --       figPath = "report/l2_vs_time.png"
-- --       fig :: [[Plot]] -> Figure
-- --       fig plots = Figure figPath [] (Multiplot Figure.Row "L2 vs time" plots)
-- --   in undefined
--   -- grid
--   --   <&> ((,) <$> title <*> algoPars)
--   --   <&> (second . fmap . second) (\a -> Benchmark a Toy)
--   --   <&> (second . fmap . second) (replicateBench replications)
-- 
-- 
-- 
report :: X [Figure]
report = sequenceA
 [ pure toyModelDistribution
-- , Report.steps
 ]
--  , steps
--  , l2VsNSimus
--  , epsilonVSCPUTime
--  , epsilonVSRealTime
--  , l2VSCPUTime
--  -- , l2VSRealTime
--  , pAccVSCPUTime
--  , pAccVSRealTime
--  --, l2VsTime
--  ]

