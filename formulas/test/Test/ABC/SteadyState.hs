{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Test.ABC.SteadyState where

import Protolude

import Test.QuickCheck
import Test.QuickCheck.Assertions

import Distribution
import Formulas
import Test.Util

import ABC.SteadyState

runTests = do
  quickCheck True
