{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}


module VSTime where

import Protolude 

import Control.Monad.Random.Lazy
import Data.Cached
import Data.List (zipWith4)
import Data.Functor.Compose
import Data.Text (pack, unpack, unlines)

import Algorithm
import Figure
import Replications
import qualified Steps
import Util


data Point = Point {_time :: Double
                   ,_realTime :: Double
                   ,_epsilon :: Double
                   ,_pAcc :: Double
                   ,_l2 :: Double}
  deriving (Eq, Show)

newtype VSTime = VSTime [Point]
  deriving (Eq, Show)

vsTime :: Algorithm -> Steps.StepsResult -> VSTime
vsTime algo r =
  VSTime $ getZipList $ Point <$>
    ZipList cpuTime <*>
    ZipList realTime <*>
    ZipList epsilons <*>
    ZipList pAccs <*>
    ZipList l2s
  where cpuTime = fromIntegral . Steps._t <$> Steps._steps r
        realTime = case algo of
                     APMC{} -> cpuTime
                     MonAPMC{getStepSize=stepSize, getParallel=par} ->
                       fmap (\t -> fromIntegral $ stepSize * (ceiling $ t / fromIntegral (par * stepSize))) cpuTime
                     _ -> panic "VSTime.vsTime.realTime Not implemented."
        epsilons = fmap (Steps._epsilon) $ Steps._steps r
        pAccs = fmap (Steps._pAcc) $ Steps._steps r
        l2s = fmap (Steps.l2Toy) $ Steps._steps r

vsTimeRep
  :: Replications
  -> Compose (Rand StdGen) Cached [VSTime]
vsTimeRep reps =
  (fmap . fmap) (vsTime (Replications._algorithm reps))
    $ cachedRepSteps "output/formulas/cached" reps

fig :: Compose (Rand StdGen) Cached ()
fig = foldr (liftA2 (<>)) (pure mempty)
      [ mkFig "report/epsilon_vs_cputime.png"
              "report/epsilon_vs_cputime.gnuplot.script"
              (unlines ["set ylabel 'epsilon'"])
              (\(VSTime ps) -> (,) <$> _time <*> _epsilon <$> ps)
      , mkFig "report/epsilon_vs_realtime.png"
              "report/epsilon_vs_realtime.gnuplot.script"
              (unlines ["set ylabel 'epsilon'"])
            (\(VSTime ps) -> (,) <$> _realTime <*> _epsilon <$> ps)
      , mkFig "report/L2_vs_cputime.png"
              "report/L2_vs_cputime.gnuplot.script"
              (unlines ["set ylabel 'l2'"])
            (\(VSTime ps) -> (,) <$> _time <*> _l2 <$> ps)
      , mkFig "report/L2_vs_realtime.png"
              "report/L2_vs_realtime.gnuplot.script"
              (unlines ["set ylabel 'l2'"])
            (\(VSTime ps) -> (,) <$> _realTime <*> _l2 <$> ps)
      , mkFig "report/pAcc_vs_cputime.png"
              "report/pAcc_vs_cputime.gnuplot.script"
              (unlines [ "set ylabel 'pAcc'"
                      , "set yrange [0:0.15]"])
            (\(VSTime ps) -> (,) <$> _time <*> _pAcc <$> ps)
      , mkFig "report/pAcc_vs_realtime.png"
              "report/pAcc_vs_realtime.gnuplot.script"
              (unlines [ "set ylabel 'pAcc'"
                       , "set yrange [0:0.15]"])
            (\(VSTime ps) -> (,) <$> _realTime <*> _pAcc <$> ps)
      ]
  where
    repsLen = Replications
                { Replications._algorithm =
                    (Algorithm.APMC 5000 500 0.01)
                , Replications._stepMax = 100
                , Replications._nReplications = 5 }
    repsMoa stepSize par = Replications
                { Replications._algorithm =
                    (Algorithm.MonAPMC
                      { getN = 5000
                      , getNAlpha = 500
                      , getPAccMin = 0.01
                      , getStepSize = stepSize
                      , getParallel = par
                      , getStopSampleSizeFactor = 5
                      })
                , Replications._stepMax = 100
                , Replications._nReplications = 5 }
    evtLen :: Compose (Rand StdGen) Cached [VSTime]
    evtLen = vsTimeRep repsLen
    evtMoa :: Int -> Int -> Compose (Rand StdGen) Cached [VSTime]
    evtMoa stepSize par = vsTimeRep (repsMoa stepSize par)
    stepSizes = [1, 2]
    parValues = [1, 2, 3, 4]
    evtMoas :: Compose (Rand StdGen) Cached [(Int, Int, [VSTime])]
    evtMoas = traverse (\(s,p) -> (s,p,) <$> evtMoa s p)
                ((,) <$> stepSizes <*> parValues)
    gnuplotScript :: FilePath -> Text
                  -> ([[(Double, Double)]], [(Int, Int, [[(Double, Double)]])])
                  -> Text
    gnuplotScript outputPath setup (dataLens, dataMoass) = unlines $
      [ "set terminal png truecolor font ',10'" :: Text
      , "set grid"
      , "set output '" <> pack outputPath <> "'"
      , "set xlabel 'time'"
      , "set yrange [0:*]"
      , "set xtics 1"
      ] <>
      pure setup <>
      -- The plot command
      pure "plot \\" <>
      pure (mconcat $ intersperse ", \\\n" $
       flip fmap dataMoass (\(stepSize, par, _) ->
         "     '-' lc " <> show (stepSize + 2) <> " w l t 'MonAPMC " <>
         show stepSize <> " " <>
         show par <> "'") <>
       pure "     '-' lc 0 w l t 'APMC'") <>
      -- Inline data
      (mconcat $ flip fmap dataMoass (\(stepSize, par, dataMoas) ->
        (intercalate ["",""] $ flip fmap dataMoas (\dataMoa ->
          flip fmap dataMoa (\(t,e) -> show t <> " " <> show e))) <>
        pure "e")) <>
      (intercalate ["", ""] $ flip fmap dataLens (\dataLen ->
        flip fmap dataLen (\(t,e) ->
          show t <> " " <> show e))) <>
      pure "e"
    mkFig :: FilePath
            -> FilePath
            -> Text
            -> (VSTime -> [(Double, Double)])
            -> Compose (Rand StdGen) Cached ()
    mkFig figPath scriptPath ylabel getData =
      liftCR
        (sinkIO figPath
          (gnuplotInline (Just $ scriptPath) .
           gnuplotScript figPath ylabel))
        ((,) <$>
         ((fmap . fmap) getData evtLen) <*>
         ((fmap . fmap . (\f (a,b,c) -> (a, b, f c)) . fmap)
           getData evtMoas))
                         
buildVSTime :: Rand StdGen (Cached ())
buildVSTime = getCompose fig

