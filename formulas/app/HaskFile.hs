{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Protolude

import           Control.Monad.Random.Lazy
import           Options.Applicative
import           Experiment
import           Util.HaskFile
import           Util.DataSet (DataSet(..))
import           Util.Repli (Repli(..))
import qualified Util.Repli as Repli
import           Util.Sample (Sample(..))

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
  | HistoRun
      FilePath -- Run
      FilePath -- Histogram
  | HistoSteps
      FilePath -- Steps
      FilePath -- Histogram for each step
  | MeanStdL2VsNSimus
      [FilePath] -- Files where replicated runs are written
      FilePath -- Mean and Std of L2 vs NSimu 
  | L2VsTime
      FilePath -- Replications
      FilePath -- L2 vs time.
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
  <> command "histo-run"
     ( info
       ( HistoRun
         <$> strOption
                ( long "run"
               <> metavar "Run")
         <*> strOption
                ( long "histo"
               <> metavar "Histograms")
       )
       mempty
     )
  <> command "histo-steps"
     ( info
       ( HistoSteps
         <$> strOption
                ( long "steps"
               <> metavar "Steps")
         <*> strOption
                ( long "histo"
               <> metavar "Histograms")
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
  <> command "l2-vs-time"
     ( info
       ( L2VsTime
         <$> argument str (metavar "Replications Steps")
         <*> argument str (metavar "L2 vs Time")
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
      res <- evalRandT (repliRun replications sim) (mkStdGen seed)
      writeSingle output res
    RepliSteps seed replications input output -> do
      sim <- readSingle input
      res <- evalRandT (repliSteps replications sim) (mkStdGen seed)
      writeSingle output res
    HistoRun pathRun pathHisto -> do
      run <- readSingle pathRun
      let histo = histogramRun run :: DataSet (Double, Double)
      writeOneFile
        ( gnuplotData2
        . gnuplotDataSetsWithNames
            [(getAlgoName $ getRunAlgo run)  <> "Steps = " <> show (getRunSteps run)]
        )
        pathHisto
        [histo]
    HistoSteps pathSteps pathHisto -> do
      steps' <- readSingle pathSteps
      let histo = histogramSteps steps' :: [DataSet (Double, Double)]
      writeOneFile
        (gnuplotData2 . gnuplotDataSetsNameWithIndex (mappend "Step " . show))
        pathHisto
        histo
    MeanStdL2VsNSimus pathsRuns pathOut -> do
      runs <- Sample <$> readList pathsRuns
        :: IO (Sample (Repli Run))
      case meanStdL2VsNSimus runs of
        Left err -> putStrLn err
        Right x -> writeOneFile gnuplotData4 pathOut x
    L2VsTime pathIn pathOut -> do
      repliSteps <- readSingle pathIn
      let stats = l2VsTimeRepliSteps repliSteps
      writeOneFile gnuplotData4 pathOut $ pureGnuplotDataSets (Repli.get stats)
    c -> panic $ "Command not implemented yet: " <> show c
    -- | RepliL2VsRealTime
    --     [FilePath] -- List of directories where each replication is written
    --     FilePath -- L2 vs real time.
    -- | RepliTvsKR
    --     [FilePath] -- List of directories containing the replications of Res.
    --     FilePath -- T MonApmc / T Apmc vs K - r

