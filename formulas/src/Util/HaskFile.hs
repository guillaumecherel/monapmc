{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Util.HaskFile where

import Protolude

import qualified Data.Text as Text
import qualified Experiment
import           Util.DataSet (DataSet(..))
import qualified Util.DataSet as DataSet

---- Functions to generate the executable functor ----

-- haskFile
--   :: (Functor m, Traversable m)
--   => (a -> m b)
--   -> ([Text] -> a)
--   -> (b -> [Text])
--   -> [FilePath]
--   -> [FilePath]
--   -> IO (m ())
-- haskFile f fromText toText inFiles outFiles = do
--   input <- traverse readFile inFiles
--   ( sequence . fmap sequence_
--     $ liftA2 writeFile outFiles
--     <$> haskText f fromText toText input )
-- 
-- haskFileIO
--   :: (a -> IO b)
--   -> ([Text] -> a)
--   -> (b -> [Text])
--   -> [FilePath]
--   -> [FilePath]
--   -> IO ()
-- haskFileIO f fromText toText inFiles outFiles = do
--   input <- traverse readFile inFiles
--   ( join . fmap sequence_
--     $ liftA2 writeFile outFiles
--     <$> haskText f fromText toText input )
-- 
-- haskText
--   :: (Functor m)
--   => (a -> m b)
--   -> ([Text] -> a)
--   -> (b -> [Text])
--   -> [Text]
--   -> m [Text]
-- haskText f fromText toText = fmap toText . f . fromText

---- Read and Write functions based on haskells Show and Read instances ----

--   [Simu Apmc Toy, Simu MonApmc Toy]
--   [Steps Apmc Toy, Steps MonApmc Toy]
--   [Simu Apmc1 Toy, ..., Simu MonApmc1 Toy, ...]
--   [Repli Res Apmc1 Toy, ..., Repli Res MonApmc1 Toy, ...]
--   [Simu MonApmc(K=k) Toy | k=1,2,4]
--   [Repli Steps Res MonApmc(K=k) | k=1,2,4]
--   [Apmc(K,N,Nalpha,pAccMin), MonApmc(K,N,Nalpha,pAccMin) 
--     | Varying N, Nalpha, pAccMin]
--   [Repli Res Apmc(K,N,Nalpha,pAccMin), Repli Res MonApmc(K,N,Nalpha,pAccMin)
--     | Varying N, Nalpha, pAccMin]
--   [Steps Apmc Toy, Steps MonApmc Toy]
--   [Repli Res Apmc1 Toy, ..., Repli Res MonApmc1 Toy, ...]
--   [Repli Steps Res MonApmc(K=k) | k=1,2,4]
--   [Repli Res Apmc(K,N,Nalpha,pAccMin), Repli Res MonApmc(K,N,Nalpha,pAccMin)
--     | Varying N, Nalpha, pAccMin]

-- Read --

readList :: forall a. (Read a) => [FilePath] -> IO [a]
readList filePaths = traverse readSingle filePaths

writeList :: forall a. (Show a) => [FilePath] -> [a] -> IO ()
writeList = writeListWith show

readSingle :: forall a. (Read a) => FilePath -> IO a
readSingle f = do
  content <- readFile f
  case readMaybe $ Text.unpack content of
    Nothing -> panic ("readSingle: Could not read file " <> Text.pack f)
    Just x -> return x

writeSingle :: forall a. (Show a) => FilePath -> a -> IO ()
writeSingle = writeOneFile show


-- Write --

writeListWith :: forall a. (a -> Text) -> [FilePath] -> [a] -> IO ()
writeListWith toText filePaths xs =
  fold $ liftA2 (writeOneFile toText) (ZipList filePaths) (ZipList xs)

writeOneFile :: (a -> Text) -> FilePath -> a -> IO ()
writeOneFile toText path x = writeFile path $ toText x

gnuplotData :: (a -> Text) -> [(Maybe Text, DataSet a)] -> Text
gnuplotData formatRecord dataSets =
     Text.intercalate "\n\n"
     ( flip fmap dataSets (\ (comment,ds)
     -> "\"" <> fromMaybe mempty comment <> "\"\n"
     <> (Text.unlines $ fmap formatRecord $ DataSet.get ds )))

gnuplotData2
  :: [(Maybe Text, DataSet (Double, Double))]
  -> Text
gnuplotData2 = gnuplotData gnuplotRecord2

gnuplotData4
  :: [(Maybe Text, DataSet (Double, Double, Double, Double))]
  -> Text
gnuplotData4 = gnuplotData gnuplotRecord4

gnuplotRecord2 :: (Double, Double) -> Text
gnuplotRecord2 (x, y) = show x <> " " <> show y

gnuplotRecord4 :: (Double, Double, Double, Double) -> Text
gnuplotRecord4 (x, y, xdelta, ydelta) =
  show x <> " " <> show y <> " " <> show xdelta <> " " <> show ydelta

pureGnuplotDataSet
  :: DataSet a
  -> [(Maybe Text, DataSet a)]
pureGnuplotDataSet dataSet = [(Nothing, dataSet)]

pureGnuplotDataSets
  :: [DataSet a]
  -> [(Maybe Text, DataSet a)]
pureGnuplotDataSets dataSets = (Nothing, ) <$> dataSets

gnuplotDataSetsWithName
  :: (Int -> Text)
  -> [DataSet a]
  -> [(Maybe Text, DataSet a)]
gnuplotDataSetsWithName name dataSets =
  zip (fmap (pure . name) [(1::Int)..]) dataSets

