{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude 

import Data.Cached
import Data.Functor.Compose
import Control.Monad.Random.Lazy

import qualified Report

main :: IO ()
main = do
  ran <- getStdGen
  putStrLn ("Constructing cache system." :: Text)
  let c = flip evalRand ran $ getCompose Report.report
  prettyCached c >>= putStrLn
  putStrLn ("Running cache system." :: Text)
  runShake ".shake" c

