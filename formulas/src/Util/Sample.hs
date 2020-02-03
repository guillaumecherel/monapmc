{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Util.Sample where

import Protolude

import qualified Data.List as List
import qualified Control.Foldl as Fold
import           Control.Foldl (Fold)

newtype Sample a = Sample [a]
  deriving (Show, Read)

get :: Sample a -> [a]
get (Sample xs) = xs

map :: (a -> b) -> Sample a -> Sample b
map f (Sample xs) = Sample (f <$> xs)

fold :: (b -> a -> b) -> b -> Sample a -> b
fold f z (Sample xs) = List.foldl' f z xs

traverse :: (Applicative f) => (a -> f b) -> Sample a -> f (Sample b)
traverse f (Sample xs) = Sample <$> Protolude.traverse f xs

append :: Sample a -> Sample a -> Sample a
append (Sample as) (Sample bs) = Sample (as <> bs)

zip :: (a -> b -> c) -> [a] -> Sample b -> Sample c
zip f xs (Sample ys) = Sample $ zipWith f xs ys



