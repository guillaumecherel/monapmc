{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Util.DataSet where

import Protolude

import           Util (nestFold)
import qualified Control.Foldl as Fold
import           Control.Foldl (Fold)

data DataSet a = DataSet [a]
  deriving (Read, Show)

get :: DataSet a -> [a]
get (DataSet xs) = xs

map :: (a -> b) -> DataSet a -> DataSet b
map f (DataSet xs) = DataSet $ fmap f xs

size :: DataSet a -> Int
size (DataSet xs) = length xs

append :: DataSet a -> DataSet a -> DataSet a
append (DataSet a) (DataSet b) = DataSet $ a <> b

meanStd
  :: [DataSet (Double, Double)] 
  -> Either Text (DataSet (Double, Double, Double, Double))
meanStd dataSets =
    DataSet . getZipList
  . Fold.fold (nestFold meanStdPoints)
  . fmap (ZipList . Util.DataSet.get)
  <$> checkLength dataSets
  where
    meanStdPoints :: Fold (Double, Double) (Double, Double, Double, Double)
    meanStdPoints =
      (,,,)
      <$> Fold.premap fst Fold.mean
      <*> Fold.premap snd Fold.mean
      <*> Fold.premap fst Fold.std
      <*> Fold.premap snd Fold.std
    checkLength :: [DataSet a] -> Either Text [DataSet a]
    checkLength [] = Right []
    checkLength (x:[]) = Right [x]
    checkLength (x:xs) = if all (size x ==) (size <$> xs)
      then Right (x:xs)
      else Left "DataSets.meanStd: dataSets sizes differ. All dataSets must have the same size."

