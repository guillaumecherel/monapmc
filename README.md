~~~~{.haskell file="formulas/app/Main.hs"}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude 

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import Steps
import L2VSNSimus

main :: IO ()
main = shakeArgs shakeOptions{shakeVerbosity=Normal,
                              shakeColor=True} $ do
  want $ [ "report/5steps.png"
         , "report/L2_vs_nsimus.png"
         ] 

  buildSteps
  -- buildLenormand2012Steps
  -- buildSteadyStateSteps
  -- buildEasyABCSteps
  -- buildHistograms
  -- buildFigurePosteriorSteps
  -- buildFigureL2VSNSimu
  -- buildL2VSNSimu 
~~~~~~~~

