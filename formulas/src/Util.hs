{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Util where

import Protolude

import Control.Monad.Random.Lazy
import Data.Functor.Compose
import qualified Util.Figure as Figure
import           Util.Figure (Figure(..))
import qualified Data.Cached as Cached
import           Data.Cached (Cached)

type Weight = Double

liftC :: (Applicative h)
  => (f a -> g b) -> Compose h f a -> Compose h g b
liftC f = Compose . liftA f . getCompose

liftC2 :: (Applicative g)
  => (f1 a -> f2 b -> f3 c)
  -> Compose g f1 a
  -> Compose g f2 b
  -> Compose g f3 c
liftC2 f a b = Compose $ liftA2 f (getCompose a) (getCompose b)

makeFigure
  :: (Applicative f)
  => FilePath
  -> Compose f Cached Figure
  -> Compose f Cached ()
makeFigure figPath fig = liftC (Cached.sinkIO figPath Figure.gnuplotInline) fig
