{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ToyModel where

import Protolude 

import qualified Model

sample :: [Double]
sample = [-5,-4.9..5]

density :: [Double]
density = fmap (Model.toyPosterior 0) sample

cdf :: [Double]
cdf = fmap (Model.toyPosteriorCDF 0) sample

