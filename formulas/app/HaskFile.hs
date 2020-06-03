{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Protolude

import           Data.Streaming.Filesystem (openDirStream, readDirStream, closeDirStream)
import           Streamly
import           Streamly.Prelude ((|:), nil)
import qualified Streamly.Prelude as S
import           Control.Monad.Random.Lazy
import qualified Control.Foldl as Fold
import           Control.Foldl (Fold)
import           Options.Applicative
import           Experiment
import           System.Directory (listDirectory)
import           System.FilePath ((</>))
import           System.IO (hClose)
import           Text.Parsec (parse)
import           Util (strictlyPositive, getStrictlyPositive)
import qualified Util.CSV as Csv
import qualified Util.DataSet as DataSet
import           Util.HaskFile
import           Util.DataSet (DataSet(..))
import           Util.Repli (Repli(..))
import qualified Util.Repli as Repli
import           Util.Sample (Sample(..))
import           Util.Parser (simulationFileName, compFileName)


data Cmd
  = Run
      Int        -- Seed
      Text       -- Simulation
      FilePath   -- Run
  | Steps
      Int        -- Seed
      Text       -- Simulation
      FilePath   -- Steps
  | RepliRun
      Int        -- Seed
      Int        -- Replications
      Text       -- Simulation
      FilePath   -- Run
  | RepliSteps
      Int        -- Seed
      Int        -- Replications
      Text       -- Simulation
      FilePath   -- Steps
  | Comp
      Int        -- Seed
      Text       -- Comp spec
      FilePath   -- Comp value output file
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
  | StatsCompLhs 
      FilePath -- Comp Lhs directory
      FilePath -- Stats Comp LHS.
  | DistTimeRatioLhsKV
      FilePath -- Stats Comp LHS. 
      FilePath -- Time Ratio LHS KV.
  | DistTimeRatioLhsKNGen
      FilePath -- Stats Comp LHS. 
      FilePath -- Time Ratio LHS K nGen.
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
  <> command "comp"
     ( info
       ( Main.Comp
         <$> argument auto (metavar "Seed")
         <*> argument str (metavar "Comp")
         <*> argument str (metavar "FilePath")
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
  <> command "stats-comp-lhs"
     ( info
       ( StatsCompLhs 
         <$> argument str (metavar "Comp input dir")
         <*> argument str (metavar "Stats Comp Lhs")
       )
       mempty
     )
  <> command "dist-time-ratio-lhs-k-v"
     ( info
       ( Main.DistTimeRatioLhsKV 
         <$> argument str (metavar "StatsCompLhs")
         <*> argument str (metavar "DistTimeRatioLhsKV")
       )
       mempty
     )
  <> command "dist-time-ratio-lhs-k-nGen"
     ( info
       ( Main.DistTimeRatioLhsKNGen 
         <$> argument str (metavar "StatsCompLhs")
         <*> argument str (metavar "DistTimeRatioLhsKNGen")
       )
       mempty
     )
   )

parseOpts :: IO Cmd
parseOpts = execParser
  $ info (helper <*> cmd)
  ( fullDesc )

parseSimulationSpecStringOrPanic :: Text -> Simulation
parseSimulationSpecStringOrPanic txt =
  case parse simulationFileName "" txt of
    (Left e) -> panic ("Couldn't parse simulation spec " <> txt <> ": " <> show e)
    (Right sim) -> sim

parseCompSpecStringOrPanic txt =
  case parse compFileName "" txt of
    (Left e) -> panic ("Couldn't parse comp spec " <> txt <> ": " <> show e)
    (Right x) -> x

main :: IO ()
main = do
  cmd' <- parseOpts
  case cmd' of
    Main.Run seed input output -> do
      let sim = parseSimulationSpecStringOrPanic input
      res <- evalRandT (run sim) (mkStdGen seed)
      writeSingle output res
    Main.Steps seed input output -> do
      let sim = parseSimulationSpecStringOrPanic input
      res <- evalRandT (steps sim) (mkStdGen seed)
      writeSingle output res
    RepliRun seed replications input output -> do
      let sim = parseSimulationSpecStringOrPanic input
      res <- evalRandT (repliRun replications sim) (mkStdGen seed)
      writeSingle output res
    RepliSteps seed replications input output -> do
      let sim = parseSimulationSpecStringOrPanic input
      res <- evalRandT (repliSteps replications sim) (mkStdGen seed)
      writeSingle output res
    Main.Comp seed input output -> do
      let compParams = parseCompSpecStringOrPanic input
      res <- evalRandT (comp compParams) (mkStdGen seed)
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
    StatsCompLhs dirIn pathOut -> do
      let header = "nGen, nAlpha, pAccMin, parallel, stepMax, biasFactor, meanRunTime, varRunTime, compL2Apmc, compTimeApmc, compL2MonApmc, compTimeMonApmc, compL2Ratio, compTimeRatio" :: Text
      let row !c@(StatComp (CompParams nGen nAlpha pAccMin parallel stepMax biasFactor meanRunTime varRunTime) compL2Apmc compTimeApmc compL2MonApmc compTimeMonApmc compL2Ratio compTimeRatio) = 
               show nGen 
            <> ", " <> show (getStrictlyPositive nAlpha)
            <> ", " <> show pAccMin 
            <> ", " <> show parallel 
            <> ", " <> show stepMax 
            <> ", " <> show biasFactor 
            <> ", " <> show meanRunTime 
            <> ", " <> show varRunTime 
            <> ", " <> show compL2Apmc 
            <> ", " <> show compTimeApmc 
            <> ", " <> show compL2MonApmc 
            <> ", " <> show compTimeMonApmc 
            <> ", " <> show compL2Ratio 
            <> ", " <> show compTimeRatio :: Text
      dirStream <- openDirStream dirIn
      -- files
      S.unfoldrM (\_ -> do
                   mpath <- readDirStream dirStream 
                   case mpath of
                     Nothing -> closeDirStream dirStream >> return Nothing
                     Just path -> return $ Just (dirIn </> path, ()))
                 ()
        & asyncly
        -- Transform to rows 
        & S.mapM (fmap (row . statsComp) . readSingle)
        -- Add header to the head of the stream
        & S.cons header
        -- Write each line to file          
        & S.foldxM (\h l -> hPutStrLn h l >> return h) 
                   (openFile pathOut WriteMode) 
                   (\h -> hClose h >> return ())
    Main.DistTimeRatioLhsKV pathIn pathOut -> do
      rows <- readFile pathIn
            <&> Csv.decodeText
            <&> either 
                  (\err -> panic $ "Could not parse CSV " <> show pathIn <> ": " <> err )
                  identity
      let mkStatComp (nGen, nAlpha, pAccMin, parallel, stepMax, biasFactor, meanRunTime, varRunTime, compL2Apmc, compTimeApmc, compL2MonApmc, compTimeMonApmc, compL2Ratio, compTimeRatio) = 
            StatComp (CompParams nGen (strictlyPositive nAlpha) pAccMin parallel stepMax biasFactor meanRunTime varRunTime) compL2Apmc compTimeApmc compL2MonApmc compTimeMonApmc compL2Ratio compTimeRatio
      let stats = toList $ mkStatComp <$> rows
      writeOneFile gnuplotData4 pathOut $ pureGnuplotDataSets
        $ (fmap . DataSet.map) 
            (\(Experiment.DistTimeRatioLhsKV k v min mean) -> (k, v, min, mean))
        $ distTimeRatioLhsKV $ timeRatioLhsKV 100 5 stats 
    Main.DistTimeRatioLhsKNGen pathIn pathOut -> do
      rows <- readFile pathIn
            <&> Csv.decodeText
            <&> either 
                  (\err -> panic $ "Could not parse CSV " <> show pathIn <> ": " <> err )
                  identity
      let mkStatComp (nGen, nAlpha, pAccMin, parallel, stepMax, biasFactor, meanRunTime, varRunTime, compL2Apmc, compTimeApmc, compL2MonApmc, compTimeMonApmc, compL2Ratio, compTimeRatio) = 
            StatComp (CompParams nGen (strictlyPositive nAlpha) pAccMin parallel stepMax biasFactor meanRunTime varRunTime) compL2Apmc compTimeApmc compL2MonApmc compTimeMonApmc compL2Ratio compTimeRatio
      let stats = toList $ mkStatComp <$> rows
      writeOneFile gnuplotData4 pathOut $ pureGnuplotDataSets
        $ (fmap . DataSet.map) 
            (\(Experiment.DistTimeRatioLhsKNGen k nGen min mean) -> (k, nGen, min, mean))
        $ distTimeRatioLhsKNGen $ timeRatioLhsKNGen 100 (10^5 / 10) stats 
    c -> panic $ "Command not implemented yet: " <> show c

