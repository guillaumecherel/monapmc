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


-- Construct gnuplot format from DataSets --

gnuplotData :: (a -> Text) -> [(Maybe Text, [DataSet a])] -> Text
gnuplotData formatRecord dataSets =
     Text.intercalate "\n\n"
     ( flip fmap dataSets (\ (comment,dss)
     -> maybe mempty (\title -> "\"" <> title  <> "\"\n") comment
     <> Text.intercalate "\n" 
        ( flip fmap dss (\ds 
        -> Text.unlines $ fmap formatRecord $ DataSet.get ds ))))

gnuplotData2
  :: [(Maybe Text, [DataSet (Double, Double)])]
  -> Text
gnuplotData2 = gnuplotData gnuplotRecord2

gnuplotData3
  :: (Show a, Show b, Show c)
  => [(Maybe Text, [DataSet (a, b, c)])]
  -> Text
gnuplotData3 = gnuplotData gnuplotRecord3

gnuplotData4
  :: [(Maybe Text, [DataSet (Double, Double, Double, Double)])]
  -> Text
gnuplotData4 = gnuplotData gnuplotRecord4

gnuplotRecord2 :: (Double, Double) -> Text
gnuplotRecord2 (x, y) = show x <> " " <> show y

gnuplotRecord3 :: (Show a, Show b, Show c) => (a, b, c) -> Text
gnuplotRecord3 (x, y, z) = show x <> " " <> show y <> " " <> show z

gnuplotRecord4 :: (Double, Double, Double, Double) -> Text
gnuplotRecord4 (x, y, xdelta, ydelta) =
  show x <> " " <> show y <> " " <> show xdelta <> " " <> show ydelta

pureGnuplotDataSet
  :: DataSet a
  -> [(Maybe Text, [DataSet a])]
pureGnuplotDataSet dataSet = [(Nothing, [dataSet])]

pureGnuplotDataSetChunks
  :: [DataSet a]
  -> [(Maybe Text, [DataSet a])]
pureGnuplotDataSetChunks dataSet = [(Nothing, dataSet)]

pureGnuplotDataSets
  :: [DataSet a]
  -> [(Maybe Text, [DataSet a])]
pureGnuplotDataSets dataSets = (Nothing, ) . pure <$> dataSets

gnuplotDataSetsNameWithIndex
  :: (Int -> Text)
  -> [DataSet a]
  -> [(Maybe Text, [DataSet a])]
gnuplotDataSetsNameWithIndex name dataSets =
  zip (pure . name <$> [(1::Int)..]) (pure <$> dataSets)

gnuplotDataSetsWithNames
  :: [Text]
  -> [DataSet a]
  -> [(Maybe Text, [DataSet a])]
gnuplotDataSetsWithNames names dataSets =
  zip (fmap pure names) (pure <$> dataSets)

