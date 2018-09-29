{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Figure where

import Protolude
import Data.String (String)
import System.Process

import Util.Cache

gnuplot :: FilePath -> FilePath -> [(String,FilePath)] -> Sink
gnuplot output script args = Sink output command (script : fmap snd args)
  where command = callProcess ("gnuplot" :: String) gpArgs
        gpArgs = [ ("-e" :: String), "outputPath='" <> output <> "'" ]
              <> join ( fmap (\(arg,val) -> ["-e", arg <> "='" <> val <> "'"]) 
                             args )
              <> ["-c", script] 

