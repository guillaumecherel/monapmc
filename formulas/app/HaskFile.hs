{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Protolude

import           Control.Monad.Random.Lazy
import           Options.Applicative
import           Simulation
import           Util.HaskFile

data Cmd
  = Run
      Int        -- Seed
      FilePath   -- Simulation
      FilePath   -- Run
  | Steps
      Int        -- Seed
      FilePath   -- Simulation
      FilePath   -- Steps
  | RepliRun
      Int        -- Seed
      Int        -- Replications
      FilePath   -- Simulation
      FilePath   -- Run
  | RepliSteps
      Int        -- Seed
      Int        -- Replications
      FilePath   -- Simulation
      FilePath   -- Steps
  | HistoSteps
      [FilePath] -- Steps
      [FilePath] -- Histograms for each algorithm
  | MeanStdL2VsNSimus
      [FilePath] -- Files where replicated runs are written
      FilePath -- Mean and Std of L2 vs NSimu 
  | RepliL2VsRealTime
      [FilePath] -- List of directories where each replication is written
      FilePath -- L2 vs real time.
  | RepliTvsKR
      [FilePath] -- List of directories containing the replications of Res.
      FilePath -- T MonApmc / T Apmc vs K - r
  deriving (Show)

cmd :: Parser Cmd
cmd = subparser
   ( command "run"
     ( info
       ( Main.Run
         <$> argument auto (metavar "Seed")
         <*> argument str (metavar "Simulation")
         <*> argument str (metavar "Run")
       )
       mempty
     )
  <> command "steps"
     ( info
       ( Main.Steps
         <$> argument auto (metavar "Seed")
         <*> argument str (metavar "Simulation")
         <*> argument str (metavar "Run")
       )
       mempty
     )
  <> command "repli-run"
     ( info
       ( RepliRun
         <$> argument auto (metavar "Seed")
         <*> argument auto (metavar "Replications")
         <*> argument str (metavar "Simulation")
         <*> argument str (metavar "Run")
       )
       mempty
     )
  <> command "repli-steps"
     ( info
       ( RepliSteps
         <$> argument auto (metavar "Seed")
         <*> argument auto (metavar "Replications")
         <*> argument str (metavar "Simulation")
         <*> argument str (metavar "Run")
       )
       mempty
     )
  <> command "histo-steps"
     ( info
       ( HistoSteps
         <$> some
             ( strOption
                ( long "steps"
               <> metavar "Steps...")
             )
         <*> some
             ( strOption
                ( long "histo"
               <> metavar "Histograms...")
             )
       )
       mempty
     )
  <> command "mean-std-l2-vs-nsimus"
     ( info
       ( MeanStdL2VsNSimus
         <$> some
             ( strOption
                ( long "run"
               <> metavar "Run replications...")
             )
         <*> strOption
              ( long "out"
             <> metavar "Gnuplot Data File")
       )
       mempty
     )
   )

parseOpts :: IO Cmd
parseOpts = execParser
  $ info (helper <*> cmd)
  ( fullDesc )

main :: IO ()
main = do
  cmd' <- parseOpts
  case cmd' of
    Main.Run seed input output -> do
      sim <- readSingle input
      res <- evalRandT (run sim) (mkStdGen seed)
      writeSingle output res
    Main.Steps seed input output -> do
      sim <- readSingle input
      res <- evalRandT (steps sim) (mkStdGen seed)
      writeSingle output res
    RepliRun seed replications input output -> do
      sim <- readSingle input
      res <- evalRandT (repliM replications $ run sim) (mkStdGen seed)
      writeSingle output res
    RepliSteps seed replications input output -> do
      sim <- readSingle input
      res <- evalRandT (repliM replications $ steps sim) (mkStdGen seed)
      writeSingle output res
    HistoSteps pathsSteps pathsHisto -> do
      steps' <- readList pathsSteps
      let histos = fmap histogramSteps steps' :: [[DataSet (Double, Double)]]
      writeHistoSteps pathsHisto histos
    MeanStdL2VsNSimus pathsRuns pathOut -> do
      runs <- Sample <$> readList pathsRuns
        :: IO (Sample (Repli Run))
      case meanStdL2VsNSimus runs of
        Left err -> putStrLn err
        Right x -> writeL2VSNSimu pathOut x
    c -> panic $ "Command not implemented yet: " <> show c
    -- | RepliL2VsRealTime
    --     [FilePath] -- List of directories where each replication is written
    --     FilePath -- L2 vs real time.
    -- | RepliTvsKR
    --     [FilePath] -- List of directories containing the replications of Res.
    --     FilePath -- T MonApmc / T Apmc vs K - r

