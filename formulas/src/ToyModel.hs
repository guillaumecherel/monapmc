{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ToyModel where

import Protolude 

import Control.Monad.Random.Lazy
import Data.Cached as Cached
import Util.CSV

import Figure
import Model

sample :: [Double]
sample = [-5,-4.9..5]

density :: [Double]
density = fmap (toyPosterior 0) sample

cdf :: [Double]
cdf = fmap (toyPosteriorCDF 0) sample

fig :: Cached ()
fig = 
  let pathDen = "output/formulas/cached/toy_model/den.csv"
      pathCDF = "output/formulas/cached/toy_model/cdf.csv"
      csv path cols = sink path (encodeText ["x", "y"]) (pure cols)
      dataDen = zip sample density
      dataCDF = zip sample cdf
  in  liftA2 (<>) (gnuplot "report/toy_model_distribution.gnuplot"
                           "report/toy_model_distribution.png"
                           [("density", pathDen), ("cdf", pathCDF)])
      $ liftA2 (<>) (csv pathDen dataDen)
      (csv pathCDF dataCDF)

buildToyModel :: Rand StdGen (Cached ())
buildToyModel = pure fig

