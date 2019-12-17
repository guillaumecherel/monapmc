{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Execution where

import Protolude

import qualified Data.List.NonEmpty as NonEmpty
import Test.QuickCheck
import Test.QuickCheck.Assertions

import Test.Util

import MonPar

-- Simple test of the monoid parallel algorithm where `f x = 1` and
-- `<> = +`. The resulting algorithm simply counts the number of function
-- applications until it reaches `max`.
prop_count
  :: Positive Int -> Positive Int -> NonNegative Int
  -> Property
prop_count
  (Positive stepSize)
  (Positive parallel)
  (NonNegative maxSum) =
  ioProperty $ do
    res <- scanPlasticPar setup step stop stepSize
    return $ fmap getSum res ?== [1..max 1 maxSum]
  where setup :: IO (NonEmpty (Sum Int))
        setup = return $ NonEmpty.fromList $ replicate parallel $ Sum 1
        step :: IO (Sum Int) -> IO (Sum Int)
        step = fmap (const (Sum 1))
        stop :: IO (Sum Int) -> IO Bool
        stop = fmap (>= Sum maxSum)

runTests = do
  checkOrExit "prop_count" prop_count

