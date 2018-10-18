{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.ABC.SteadyState where

import Protolude

import Test.QuickCheck
import Test.QuickCheck.Assertions

import Test.Util

import ABC.SteadyState

runTests = do
  checkOrExit "True" True
