{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ToyModel where

import Protolude 

import Control.Monad.Random.Lazy
import Data.Cached as Cached
import Data.Functor.Compose
import Data.Text (unpack, unlines)
import Util.CSV

import Figure
import Model

sample :: [Double]
sample = [-5,-4.9..5]

density :: [Double]
density = fmap (toyPosterior 0) sample

cdf :: [Double]
cdf = fmap (toyPosteriorCDF 0) sample

fig :: Rand StdGen (Cached ())
fig = 
  let outputPath = "report/toy_model_distribution.png"
      dataDen = zip sample density
      dataCDF = zip sample cdf
      gnuplotScript = unlines $
        [ "set terminal png"
        , "set output '" <> outputPath <> "'"
        , "plot '-' u 1:2 with lines t 'density', \\"
        , "     '-' u 1:2 with lines t 'cdf'"
        ]
        <> flip fmap dataDen
            (\(x,y) -> show x <> " " <> show y)
        <> ["e"]
        <> flip fmap dataCDF
            (\(x,y) -> show x <> " " <>  show y)
        <> ["e"]
  in pure $
       (trigger (unpack outputPath)
         (gnuplotInline (Just "report/toy_model_distribution.gnuplot.script")
           gnuplotScript)
         mempty)

buildToyModel :: Rand StdGen (Cached ())
buildToyModel = fig

