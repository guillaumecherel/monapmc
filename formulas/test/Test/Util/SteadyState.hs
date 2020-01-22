{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Util.SteadyState where

import Protolude

import Data.List (nub)
import qualified Data.List.NonEmpty as NonEmpty

import Test.QuickCheck
import Test.QuickCheck.Assertions

import Util.Distribution
import Test.Util

import Util.SteadyState


-- Test model:
ssr :: Int -> Int -> Int -> SteadyStateRunner IO Int Int
ssr delay parallel max = steadyStateRunner f (return (s, xs)) update
  where f :: Int -> IO Int
        f x = threadDelay delay >> return x
        s :: Int
        s = 0
        xs :: NonEmpty Int
        xs = NonEmpty.fromList $ replicate parallel 1
        update :: (Int, Int) -> IO (Maybe (Int, Int))
        update (s, y) = if s < max
                          then return $ Just (s + y, 1)
                          else return Nothing


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
prop_steadyStateParallelRun :: Property
prop_steadyStateParallelRun =
  within 400000 $ ioProperty $ do
    res <- run $ ssr 50000 2 10
    return $ res ?== 10

prop_steadyStateParallelScan :: Property
prop_steadyStateParallelScan =
  within 400000 $ ioProperty $ do
    res <- scan $ ssr 50000 2 10
    return $ res ?== [0..10]

prop_run :: (Positive Int) -> NonNegative Int -> Property
prop_run (Positive parallel) (NonNegative n) =
  ioProperty $ do
    res <- run (ssr 0 parallel n)
    return $ res ?== n

prop_scan :: (Positive Int) -> NonNegative Int -> Property
prop_scan (Positive parallel) (NonNegative n) =
  ioProperty $ do
    res <- scan (ssr 0 parallel n)
    return $ res ?== [0..n]

prop_runN :: NonNegative Int -> Positive Int -> NonNegative Int -> Property
prop_runN (NonNegative nRunN) (Positive parallel) (NonNegative nSSR) =
  ioProperty $ do
    res <- runN nRunN (ssr 0 parallel nSSR)
    return $ res ?== min nRunN nSSR

prop_scanN :: NonNegative Int -> Positive Int -> NonNegative Int -> Property
prop_scanN (NonNegative nScanN) (Positive parallel) (NonNegative nSSR) =
  ioProperty $ do
    res <- scanN nScanN (ssr 0 parallel nSSR)
    return $ res ?== [0..min nScanN nSSR]

newtype StrictlyIncreasing = StrictlyIncreasing [Int]
  deriving (Show, Eq)

instance Arbitrary StrictlyIncreasing where
  arbitrary = do
    l <- arbitrary :: Gen (OrderedList (NonNegative Int))
    return $ StrictlyIncreasing $ nub $ fmap getNonNegative $ getOrdered l

prop_scanIndices :: StrictlyIncreasing -> Positive Int
                 -> NonNegative Int -> Property
prop_scanIndices (StrictlyIncreasing indices) (Positive parallel) (NonNegative nSSR) =
  ioProperty $ do
    res <- scanIndices indices (ssr 0 parallel nSSR)
    return $ collect indices $ res ?== filter (<= nSSR) indices

runTests = do
  checkOrExit "prop_RunningRuns" prop_RunningRuns
  checkOrExit "prop_steadyStateParallelRun" prop_steadyStateParallelRun
  checkOrExit "prop_steadyStateParallelScan" prop_steadyStateParallelScan
  checkOrExit "prop_run" prop_run
  checkOrExit "prop_scan" prop_scan
  checkOrExit "prop_runN" prop_runN
  checkOrExit "prop_scanN" prop_scanN
  checkOrExit "prop_scanIndices" prop_scanIndices
