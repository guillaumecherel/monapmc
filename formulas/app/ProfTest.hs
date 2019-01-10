{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude 

import Data.List (last)
import Data.Cached
import qualified Data.Vector as V
import Control.Parallel
import Control.Monad.Random.Lazy
import Development.Shake hiding (par)


import qualified Algorithm
import Model
import L2VSNSimus
import Steps
import qualified Run
import qualified ABC.Lenormand2012 as Lenormand2012

main :: IO ()
main = do
  res <- evalRand (Run.run 100 (Algorithm.Lenormand2012 5000 0.1 0.01)) (mkStdGen 1)
  putStrLn $ (show (Run._stepCount res) :: Text)

