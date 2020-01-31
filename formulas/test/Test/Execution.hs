{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Execution where

import Protolude

import qualified Data.List as List
import Control.Monad.Random.Lazy 
import qualified Data.List.NonEmpty as NonEmpty
import Test.QuickCheck
import Test.QuickCheck.Assertions

import Test.Util

import Util.Execution
import Util.Duration (Duration, fromSeconds, fromPicoSeconds, seconds)

-- Simple test of the monoid parallel algorithm where `f x = 1` and
-- `<> = +`. The resulting algorithm simply counts the number of function
-- applications until it reaches `max`.
prop_count :: Positive Int -> Positive Int -> NonNegative Int -> Property
prop_count
  (Positive stepSize)
  (Positive parallel)
  (NonNegative maxSum) =
  ioProperty $ do
    res <- scanPlasticPar split step stop stepSize parallel
    return $ fmap getSum res ?== [1..max 1 maxSum]
  where split :: (IO (Sum Int) -> IO (Sum Int, Sum Int))
        split ioS = do
          s <- ioS
          return (s, Sum 0)
        step :: IO (Sum Int) -> IO (Sum Int)
        step = fmap (const (Sum 1))
        stop :: IO (Sum Int) -> IO Bool
        stop = fmap (>= Sum maxSum)


-- Simple test of the easy parallelism simulator.
prop_simEasyPar :: Property
prop_simEasyPar =
  let f :: Int -> Rand StdGen (Duration, Int)
      f x = return (fromPicoSeconds (fromIntegral x) * 10 ^ 12, x)
      -- Algorithm state: (iteration, xs)
      xs = [9, 8, 7, 6, 5, 0]
      init = (0, xs)
      stepPre :: (Integer, [Int]) -> Rand StdGen (Integer, [Int])
      stepPre (iter, _) = return (iter, xs)
      stepPost
        :: (Integer, [Int])
        -> Integer 
        -> [Int]
        -> Rand StdGen (Integer, [Int])
      stepPost _ iter ys = return (iter + 1, ys)
      stop :: (Integer, [Int]) -> Bool
      stop (iter, _) = iter >= 2
      parallel = 2
      gen = mkStdGen 1
  in ioProperty $ do
     ((algoDuration, simDuration), (iter, ys)) <- evalRandT
          (List.last <$> simEasyPar stepPre f stepPost stop parallel init)
          gen
     -- Test passes if the reported duration is greater than 21 * 2 (21 per
     -- step) and less than 22 * 2, implying that the actual run time taken by
     -- the algorithm for each step (and not by the simulated model run) takes
     -- less than 1 second.
     return $ algoDuration ?< fromSeconds 0.1
         .&&. simDuration ?== (fromPicoSeconds $ 21 * 2 * 10 ^ 12)
         .&&. iter ?== 2
         .&&. ys ?== xs

-- Simple test of the monoid parallel simulator.

data TestState = TestState Int [Integer]
  -- TestState number_of_simulations_run simulation_durations
  deriving (Eq, Show)

instance NFData TestState where
  rnf (TestState n ds1) = rnf n `seq` rnf ds1

instance Semigroup TestState where
  TestState n1 ds1 <> TestState n2 ds2 = TestState (n1 + n2) (ds1 ++ ds2)
  
instance Monoid TestState where
  mempty = TestState 0 []

prop_simPlasticPar :: Property
prop_simPlasticPar =
  let
      -- Individual durations of all the simulations to run.
      durations =  [9, 8, 7, 6, 5, 0]
      fill = take 3 $ repeat (10^20)
      -- Algo state: total number of simulations already run and individual durations of simulations to run. Fill with simulations that run for a very long time.
      init = TestState 0 (durations ++ fill)
      -- The right part of the split contains the next simulation to run (its duration), while the left parts contains all the rest.
      split :: (Rand StdGen TestState -> Rand StdGen (TestState, TestState))
      split rs = do
        (TestState n ds) <- rs
        return (TestState n (tailSafe ds), TestState 0 (toList $ headMay ds))
      -- The step simply returns the duration as is and removes the current simulation from the list.
      step :: Rand StdGen TestState -> Rand StdGen (Duration, TestState)
      step rs = do
        (TestState n ds) <- rs
        return (fromPicoSeconds $ headDef 0 ds * 10 ^ 12, TestState (n + 1) (tailSafe ds))
      stop :: Rand StdGen TestState -> Rand StdGen Bool
      stop rs = fmap (\(TestState n _) -> n >= length durations) rs
      stepSize = 1
      parallel = 2
  in ioProperty $ do
     ((algoDuration, simDuration), finalState) <- evalRandT
          (List.last <$> simPlasticPar split step stop stepSize parallel init)
          (mkStdGen 1)
     -- Test passes if the reported duration is greater than 20 seconds
     -- (simulated) and less than 21 seconds, implying that the actual
     -- run time taken by the algorithm for each step (and not by the
     -- simulated model run) takes less than 1 second.
     return $ algoDuration ?<= fromSeconds 0.1
         .&&. simDuration ?== (fromPicoSeconds $ 20 * 10 ^ 12)
           -- The final state should contain the "fill" list with the first two elements dropped, because they have been sent to run.
         .&&. finalState == TestState (length durations) (drop 2 fill)

runTests = do
  checkOrExit "prop_count" prop_count
  checkOrExit "prop_simEasyPar" prop_simEasyPar
  checkOrExit "prop_simPlasticPar" prop_simPlasticPar

