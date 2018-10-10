{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.Util.SteadyState where

import Protolude

import Test.QuickCheck
import Test.QuickCheck.Assertions

import Distribution
import Formulas
import Test.Util

import Util.SteadyState

-- Checks that an IO action in a Running actually runs as soon as it is created
prop_RunningRuns :: Property
prop_RunningRuns = once $ within 200000 $ ioProperty $ do
  let wait = do
                -- putStrLn "Start inner wait"
                threadDelay 100000
                -- putStrLn "End inner wait"
  let ioRunning = mkRunning wait
  rs <- sequence $ take 50 $ repeat ioRunning
  -- putStrLn "Start do wait"
  threadDelay 100000
  -- putStrLn "End do wait"
  traverse (takeMVar . getMVar) rs
  return True

-- Check that a steady state actually runs in parallel.
prop_steadyStateParallel :: Property
prop_steadyStateParallel =
  within 100000 $ ioProperty $ do
    steadyStateRunN 10 $ steadyStateRunner f (s, xs) update
    return True
  where f x = threadDelay 100000
        s = ()
        xs = replicate 10 ()
        update (_, _) = return ((), ())

runTests = do
  checkOrExit prop_RunningRuns
  checkOrExit prop_steadyStateParallel
