~~~~{.haskell file="formulas/app/Main.hs"}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude 

import Control.Monad.Random.Lazy
import Development.Shake

import L2VSNSimus
import Steps
import Util.Cache

main :: IO ()
main = do
  ran <- getStdGen
  putStrLn ("Constructing cache system." :: Text)
  let c = mconcat $ flip evalRand ran $ sequence 
                    [ buildSteps
                    , buildL2VSNSimus
                    ]
  putStrLn (prettyCache c)
  putStrLn ("Creating Shake Rules." :: Text)
  rules <- return $ buildCache c
  putStrLn ("Running cache system." :: Text)
  shakeArgs shakeOptions{ shakeVerbosity=Normal
                        , shakeColor=True 
                        , shakeThreads=0 } 
            rules
~~~~~~~~

