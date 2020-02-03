{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Util.Repli where

import Protolude

import qualified Data.List as List
import qualified Control.Foldl as Fold
import           Control.Foldl (Fold)

data Repli a = Repli Int [a]
  deriving (Show, Read)

get :: Repli a -> [a]
get (Repli _ xs) = xs

repli :: Int -> a -> Repli a
repli n x = Repli n $ replicate n x

repliM :: (Applicative m) => Int -> m a -> m (Repli a)
repliM n x = Repli n <$> replicateM n x

map :: (a -> b) -> Repli a -> Repli b
map f (Repli n xs) = Repli n (f <$> xs)

fold :: (b -> a -> b) -> b -> Repli a -> b
fold f z (Repli n xs) = List.foldl' f z xs

traverse :: (Applicative f) => (a -> f b) -> Repli a -> f (Repli b)
traverse f (Repli i xs) = Repli i <$> Protolude.traverse f xs

aggregate :: ([a] -> b) -> Repli a -> b
aggregate f (Repli _ xs) = f xs


