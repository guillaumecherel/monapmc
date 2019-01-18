{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude
import Unsafe (unsafeHead)

import Data.List (last)
import Data.Cached
import qualified Data.Vector as V
import Control.Parallel
import Control.Monad.Random.Lazy
import qualified Numeric.LinearAlgebra as LA
import Development.Shake hiding (par)
import qualified Control.Foldl as Fold


import qualified Algorithm
import Model
import L2VSNSimus
import Steps
import qualified Run
import ABC.Lenormand2012

main :: IO ()
main = do
  replicateM 10 $ do
    g <- newStdGen
    res <- newStdGen >>= evalRand (Run.run 100 (Algorithm.Lenormand2012 5000 0.1 0.1))
    putStrLn $ (show (Run.nSimus res, Fold.fold ((,) <$> Fold.mean <*> Fold.variance) $ fmap (uncurry (*)) $ (fmap . second)  V.head $ Run._sample res) :: Text)
  return ()

