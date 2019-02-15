{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Figure where

import Protolude

-- import Control.Monad.Random.Lazy
-- import Data.Functor.Compose
import qualified Data.Set as Set
import Data.String (String)
import System.Process

import Data.Cached

-- import Util

gnuplot1
  :: FilePath -> FilePath
  -> [(String, FilePath)]
  -> [Cached [[Double]]]
  -> Cached ()
gnuplot1 = gnuplotCached (gnuplotData1 identity)

gnuplot2
  :: FilePath -> FilePath
  -> [(String, FilePath)]
  -> [Cached [[(Double, Double)]]]
  -> Cached ()
gnuplot2 = gnuplotCached (gnuplotData2 fst snd)

gnuplot3
  :: FilePath -> FilePath
  -> [(String, FilePath)]
  -> [Cached [[(Double, Double, Double)]]]
  -> Cached ()
gnuplot3 = gnuplotCached (gnuplotData3 (\(a,_,_) -> a)
                                 (\(_,a,_) -> a)
                                 (\(_,_,a) -> a))

gnuplot4
  :: FilePath -> FilePath
  -> [(String, FilePath)]
  -> [Cached [[(Double, Double, Double , Double)]]]
  -> Cached ()
gnuplot4 = gnuplotCached (gnuplotData4 (\(a,_,_,_) -> a)
                                 (\(_,a,_,_) -> a)
                                 (\(_,_,a,_) -> a)
                                 (\(_,_,_,a) -> a))

gnuplotCached :: forall a. ([[a]] -> GnuplotData)
         -> FilePath -> FilePath
         -> [(String, FilePath)]
         -> [Cached [[a]]]
         -> Cached ()
gnuplotCached gpd output script args cachedData = mconcat (fig:gpData)
  where fig = gnuplot output script args
        gpData :: [Cached ()]
        gpData = fmap sinkInputFile (zip (fmap snd args) cachedData)
        sinkInputFile :: (FilePath, Cached [[a]]) -> Cached ()
        sinkInputFile (f,c) = gnuplotDataSink f $ gpd <$> c

gnuplot :: FilePath -> FilePath -> [(String,FilePath)] -> Cached ()
gnuplot output script args = trigger output
                                     (command)
                                     (Set.fromList $ script:fmap snd args)
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

data GnuplotRow = GnuplotRow1 Double
                   | GnuplotRow2 Double Double
                   | GnuplotRow3 Double Double Double
                   | GnuplotRow4 Double Double Double Double
gnuplotRow1 :: (a -> Double) -> a -> GnuplotRow
gnuplotRow1 _1 = GnuplotRow1 . _1

gnuplotRow2 :: (a -> Double) -> (a -> Double) -> a -> GnuplotRow
gnuplotRow2 _1 _2 = GnuplotRow2 <$> _1 <*> _2

gnuplotRow3 :: (a -> Double) -> (a -> Double) -> (a -> Double)
            -> a -> GnuplotRow
gnuplotRow3 _1 _2 _3 =  GnuplotRow3 <$> _1 <*> _2 <*> _3

gnuplotRow4 :: (a -> Double) -> (a -> Double) -> (a -> Double) -> (a -> Double)
            -> a -> GnuplotRow
gnuplotRow4 _1 _2 _3 _4 = GnuplotRow4 <$> _1 <*> _2 <*> _3 <*> _4

gnuplotRowText :: GnuplotRow -> Text
gnuplotRowText (GnuplotRow1 x1) = show x1
gnuplotRowText (GnuplotRow2 x1 x2) =
  show x1 <> " " <> show x2
gnuplotRowText (GnuplotRow3 x1 x2 x3) =
  show x1 <> " " <> show x2 <> " " <> show x3
gnuplotRowText (GnuplotRow4 x1 x2 x3 x4) =
  show x1 <> " " <> show x2 <> " " <> show x3 <> " " <> show x4

newtype GnuplotData = GnuplotData [[GnuplotRow]]

gnuplotData1 :: (a -> Double) -> [[a]] -> GnuplotData
gnuplotData1 _1 = GnuplotData . (fmap . fmap) (gnuplotRow1 _1)

gnuplotData2 :: (a -> Double) -> (a -> Double) -> [[a]] -> GnuplotData
gnuplotData2 _1 _2 = GnuplotData . (fmap . fmap) (gnuplotRow2 _1 _2)

gnuplotData3 :: (a -> Double) -> (a -> Double) -> (a -> Double)
             -> [[a]] -> GnuplotData
gnuplotData3 _1 _2 _3 = GnuplotData . (fmap . fmap) (gnuplotRow3 _1 _2 _3)

gnuplotData4 :: (a -> Double) -> (a -> Double) -> (a -> Double)
             -> (a -> Double) -> [[a]] -> GnuplotData
gnuplotData4 _1 _2 _3 _4 =
  GnuplotData . (fmap . fmap) (gnuplotRow4 _1 _2 _3 _4)

gnuplotDataText :: GnuplotData -> Text
gnuplotDataText (GnuplotData xss) =
  mconcat $ intersperse "\n\n\n" $ fmap textSet xss
  where textSet :: [GnuplotRow] -> Text
        textSet xs = mconcat $ intersperse "\n" $ fmap gnuplotRowText xs

gnuplotDataSink :: FilePath -> Cached GnuplotData -> Cached ()
gnuplotDataSink path = sink path (pure . gnuplotDataText)

