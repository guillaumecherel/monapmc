{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module L2 where

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
import Util.CSV

import Util.Figure
import Statistics

sample :: [Double]
sample = [-5,-4.9..5]

density :: [Double]
density = fmap (toyPosterior 0) sample

cdf :: [Double]
cdf = fmap (toyPosteriorCDF 0) sample

fig :: Cached ()
fig = 
  let pathDen = "output/formulas/cached/l2/den.csv"
      pathCDF = "output/formulas/cached/l2/cdf.csv"
      csv path cols = sink path (pure . columns2 " ") (pure cols)
      dataDen = zip sample density
      dataCDF = zip sample cdf
  in  liftA2 (<>) (gnuplot "report/toy_model_distribution.gnuplot"
                           "report/toy_model_distribution.png"
                           [("density", pathDen), ("cdf", pathCDF)])
      $ liftA2 (<>) (csv pathDen dataDen)
      (csv pathCDF dataCDF)

buildToyModel :: Rand StdGen (Cached ())
buildToyModel = pure fig

