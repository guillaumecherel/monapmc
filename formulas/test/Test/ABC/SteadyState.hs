{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.ABC.SteadyState where

import Protolude

import Test.QuickCheck
import Test.QuickCheck.Assertions

import Test.Util

import ABC.SteadyState

runTests = do
  checkOrExit True
