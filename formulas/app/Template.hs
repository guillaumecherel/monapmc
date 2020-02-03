{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Protolude

import qualified Data.Text as Text
import           Options.Applicative
import           Experiment
import           Text.Pretty.Simple (pPrintNoColor)

args :: Parser Simulation
args = Simulation
  <$> subparser
        ( command "apmc"
          ( info
            ( APMC
              <$> argument auto (metavar "n")
              <*> argument auto (metavar "nAlpha")
              <*> argument auto (metavar "pAccMin")
              <*> argument auto (metavar "parallel")
            )
            mempty
          )
       <> command "mon-apmc"
          ( info
            ( MonAPMC
              <$> argument auto (metavar "n")
              <*> argument auto (metavar "nAlpha")
              <*> argument auto (metavar "pAccMin")
              <*> argument auto (metavar "stepSize")
              <*> argument auto (metavar "parallel")
              <*> argument auto (metavar "stopSampleSize")
            )
            mempty
          )
       <> command "Beaumont2009"
          ( info
            ( Beaumont2009
              <$> argument auto (metavar "n")
              <*> argument auto (metavar "epsilonFrom")
              <*> argument auto (metavar "epsilonTo")
            )
            mempty
          )
       <> command "SteadyState"
          ( info
            ( SteadyState
              <$> argument auto (metavar "n")
              <*> argument auto (metavar "alpha")
              <*> argument auto (metavar "pAccMin")
              <*> argument auto (metavar "parallel")
            )
            mempty
          )
      )
  <*> argument auto (metavar "model")
  <*> argument auto (metavar "stepMax")
 
parseOpts :: IO Simulation
parseOpts = execParser
  $ info (helper <*> args)
  ( fullDesc )

main :: IO ()
main = do
  arguments <- parseOpts
  pPrintNoColor arguments
