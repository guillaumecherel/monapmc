{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude 

import qualified Data.Cached as Cached
import Data.Functor.Compose
import Control.Monad.Random.Lazy

import qualified Report
import qualified Util.Figure as Figure

main :: IO ()
main = do
  -- ran <- getStdGen
  -- putStrLn ("Constructing cache system." :: Text)
  -- cachedFigures <- flip evalRandT ran $ getCompose Report.report
  -- let cache =
  --      Cached.trigger $ (fmap . traverse_) Figure.gnuplotInline cachedFigures
  -- Cached.runShake ".shake" cache
  -- Cached.prettyCached cache >>= putStrLn
  putStrLn ("Running cache system." :: Text)

