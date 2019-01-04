{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude 

import Data.Cached
import Control.Monad.Random.Lazy
import Development.Shake

import ToyModel
import L2VSNSimus
import Steps

main :: IO ()
main = do
  ran <- getStdGen
  putStrLn ("Constructing cache system." :: Text)
  let c = mconcat $ (flip evalRand ran $ sequence
                    [ buildToyModel
                    , buildSteps
                    , buildL2VSNSimus
                    ])
  prettyCached c >>= putStrLn
  putStrLn ("Running cache system." :: Text)
  runShake c

-- Profiling            
-- main :: IO ()
-- main = do
--   ran <- getStdGen
--   let c = mconcat $ flip evalRand ran $ sequence 
--           $ (fmap . fmap . fmap) (\_ -> ()) 
--             [ steadyState 0.2 0.01 1
--             ]
--             -- , steadyState 0.1 0.01 2
--             -- , steadyState 0.1 0.05 1
--             -- , steadyState 0.1 0.05 2 ]
--   (prettyCached c) >>= putStrLn
--   let rules = buildCache c
--   shakeArgs shakeOptions{ shakeVerbosity = Normal
--                         , shakeColor = True
--                         , shakeThreads=0 }
--             rules

