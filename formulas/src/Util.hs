{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Util where

import Protolude

import Control.Monad.Random.Lazy
import Data.Functor.Compose

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

liftCR :: (Applicative f) => (f a -> f b) 
      -> Compose (Rand StdGen) f a
      -> Compose (Rand StdGen) f b
liftCR f = Compose . liftA f . getCompose

liftCR2 :: (Applicative f) => (f a -> f b -> f c)
       -> Compose (Rand StdGen) f a
       -> Compose (Rand StdGen) f b
       -> Compose (Rand StdGen) f c
liftCR2 f a b = Compose $ liftA2 f (getCompose a) (getCompose b)


