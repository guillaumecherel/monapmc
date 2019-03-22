{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module L2VSNSimus where

import Protolude 

import qualified Control.Foldl as Fold
import Control.Monad.Random.Lazy
import Data.Cached as Cached
import Data.Functor.Compose
import Data.Text (unlines, unpack)

import Algorithm
import Figure
import Replications
import Run
import Statistics
import Util

data L2VSNSimus = L2VSNSimus
  { _l2Mean :: Double
  , _l2Std :: Double
  , _nSimusMean :: Double
  , _nSimusStd :: Double
  }

l2VSNSimus :: Run -> [RunResult] -> L2VSNSimus
l2VSNSimus r reps = Fold.fold f reps
  where f = L2VSNSimus <$> l2Mean <*> l2Std <*> nSimMean <*> nSimStd
        l2Mean = foldMeanWith Run.l2Toy
        l2Std = foldStdWith Run.l2Toy
        nSimMean = foldMeanWith (fromIntegral . nSimus r)
        nSimStd = foldStdWith (fromIntegral . nSimus r)

l2VSNSimus'
  :: Int -> Int -> Algorithm
  -> Compose (Rand StdGen) Cached L2VSNSimus
l2VSNSimus' nRep stepMax algo =
  l2VSNSimus (Run algo stepMax) <$> cachedRepRuns "output/formulas/cached"
    (Replications algo stepMax nRep)

fig :: Compose (Rand StdGen) Cached ()
fig = 
  let nReplications = 10
      alphas = [1%10, 2%10 .. 9%10]
      nAlpha = 500
      pAccMins = [0.01, 0.05, 0.1, 0.2]
      stepMax = 100
      outputPath = "report/L2_vs_nsimus.png"
      len pAccMin alpha = Lenormand2012
                            { getN = floor (nAlpha / alpha)
                            , getAlpha=fromRational alpha
                            , getPAccMin=pAccMin}
      moa stepSize parallel pAccMin alpha = MonAPMC
                            { getN = floor (nAlpha / alpha)
                            , getAlpha = fromRational alpha
                            , getPAccMin = pAccMin
                            , getStepSize = stepSize
                            , getParallel = parallel}
      figData :: Compose (Rand StdGen) Cached
                 [(Text, [(Double, [(Ratio Integer, L2VSNSimus)])])]
      figData = (traverse . traverse .
                 traverse . traverse .
                 traverse . traverse)
                  (l2VSNSimus' nReplications stepMax) $
                ([("APMC", len), ("MonAPMC", moa 1 1)] &
                 fmap (\(label, algo) ->
                   (label, pAccMins & fmap (\pAccMin ->
                     (pAccMin, alphas & fmap (\alpha ->
                       (alpha, algo pAccMin alpha)))))))
      gnuplotScript :: [(Text, [(Double, [(Ratio Integer, L2VSNSimus)])])]
                    -> Text
      gnuplotScript ds = unlines $
            [ "set terminal png truecolor" :: Text
            , "set output '" <> outputPath <> "'"
            , "set grid"
            , "set key on inside horizontal"
            , "# set yrange [0:.25]"
            , "set ytics 0,.05,.2"
            , "# set xrange [0.2e5:33e5]"
            , "set logscale x 2"
            , "# set xtics (0.125e5, 0.25e5, 0.5e5, 1e5, 2e5, 4e5, 8e5, 16e5, 32e5)"
            , "set xtics 3125, 2"
            ] <>
            ["plot \\"] <>
            -- The plot command
            pure (mconcat $ intersperse ", \\\n" $
              -- Map over algorithms
              (mconcat $ flip fmap (zip [1..] ds) (\(i,(label, d)) ->
                -- Map over pAccMin
                flip fmap (zip [1..] d) (\(j,(pAccMin, _)) ->
                   "    '-' with xyerrorbars" <>
                   " linecolor " <> show (i + 2) <>
                   " pointtype " <> show j <>
                   " t '" <> label <> " " <> show pAccMin <> "'")))) <>
            -- The inline data
            -- Map over algorithms
            (mconcat $
             flip fmap ds (\(label, d) ->
              -- Map over pAccMin
              (intercalate ["e"] $
               flip fmap d (\(pAccMin, d') ->
                 -- Map over alpha
                 flip fmap d' (\(alpha, lvns) ->
                     show (_nSimusMean lvns) <> " "
                     <> show (_l2Mean lvns) <> " "
                     <> show (_nSimusStd lvns) <> " "
                     <> show (_l2Std lvns)))) <> 
              ["e"]))
  in liftCR
       (sinkIO (unpack outputPath)
         (gnuplotInline (Just "report/L2_vs_nsimus.gnuplot.script") .
          gnuplotScript))
       figData

buildL2VSNSimus :: Rand StdGen (Cached ())
buildL2VSNSimus = getCompose fig

