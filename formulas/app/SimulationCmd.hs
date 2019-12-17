{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude hiding (option)
-- 
-- import Data.Cached
-- import Data.Functor.Compose
-- import Control.Monad.Random.Lazy
-- import Options.Applicative
-- 
-- import qualified Simulation
-- 
-- data SimulationCmd =
--   Simulation
--     Algorithm -- the algorithm
--     Int -- step max
--     Int -- replications
--     FilePath -- cache path
--   deriving (Show, Eq)
-- 
-- simulation :: Parser Simulation
-- simulation = Simulation
--   <$> subparser
--     ( command "APMC" (info (helper <*> apmc) fullDesc)
--       <> command "MonAPMC" (info (helper <*> monApmc) fullDesc))
--   <*> option auto (long "step-max" <> metavar "STEPMAX")
--   <*> option auto (long "replications" <> metavar "REPLICATIONS")
--   <*> option str (long "cache-path"<> metavar "CACHEPATH")
-- 
-- apmc :: Parser Algorithm
-- apmc = Algorithm.APMC
--   <$> argument auto (metavar "N")
--   <*> argument auto (metavar "Nalpha")
--   <*> argument auto (metavar "pAccMin")
-- 
-- monApmc :: Parser Algorithm
-- monApmc = Algorithm.MonAPMC
--   <$> argument auto (metavar "N")
--   <*> argument auto (metavar "Nalpha")
--   <*> argument auto (metavar "pAccMin")
--   <*> argument auto (metavar "stepSize")
--   <*> argument auto (metavar "parallel")
--   <*> argument auto (metavar "stopSampleSizeFactor")
-- 
--  
-- parseOpts :: IO Simulation
-- parseOpts = execParser
--   $ info (helper <*> simulation)
--   ( fullDesc )
-- 
-- doReplications
--   :: Algorithm
--   -> Int
--   -> Int
--   -> Compose (Rand StdGen) Cached [StepsResult]
-- doReplications algo stepMax replications =
--   Report.repSteps (Replications algo  stepMax replications)

main :: IO ()
main = do
  undefined
  -- opts@(Simulation algo stepMax replications cachePath) <- parseOpts
  -- ran <- getStdGen
  -- putStrLn ( "Running simulation: " <> show opts :: Text)
  -- let results = flip evalRand ran $ getCompose
  --       $ doReplications algo stepMax replications
  -- prettyCached results >>= putStrLn
  -- putStrLn ("Running cache system." :: Text)
  -- runShake ".shake-simulation" results

