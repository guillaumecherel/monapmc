{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Figure where

import Protolude
import Data.String (String)
import System.Process

import Util.Cache

gnuplot :: FilePath -> FilePath -> [(String,FilePath)] -> Cache ()
gnuplot output script args = sink output (\() -> command) needs
  where command = do
          (status, out, err) <- lift
            $ readProcessWithExitCode ("gnuplot" :: String) gpArgs []
          lift $ hPutStrLn stderr err
          lift $ hPutStrLn stdout out
          case status of
            ExitSuccess -> return ()
            ExitFailure code -> throwE $
              "Error: command gnuplot exited with status " <> show code
              <> "\nFailing command: " <> "gnuplot " <> show gpArgs
        gpArgs = [ ("-e" :: String), "outputPath='" <> output <> "'" ]
              <> join ( fmap (\(arg,val) -> ["-e", arg <> "='" <> val <> "'"]) 
                             args )
              <> ["-c", script]
        needs = foldMap need $ script:(map snd args)
