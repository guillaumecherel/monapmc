{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Figure where

import Protolude

import Control.Monad.Fail
import qualified Data.Set as Set
import Data.String (String)
import System.Process

import Data.Cached

gnuplot :: FilePath -> FilePath -> [(String,FilePath)] -> Cached ()
gnuplot output script args = trigger (output  ++ ".tag")
                                     (command)
                                     (Set.fromList $ fmap snd args)
  where command = do
          (status, out, err) <- readProcessWithExitCode ("gnuplot" :: String) gpArgs []
          hPutStrLn stderr err
          hPutStrLn stdout out
          case status of
            ExitSuccess -> return $ Right ()
            ExitFailure code -> return $ Left $
              "Error: command gnuplot exited with status " <> show code
              <> "\nFailing command: " <> "gnuplot " <> show gpArgs
        gpArgs = [ ("-e" :: String), "outputPath='" <> output <> "'" ]
              <> join ( fmap (\(arg,val) -> ["-e", arg <> "='" <> val <> "'"]) 
                             args )
              <> ["-c", script]

