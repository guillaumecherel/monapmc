{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Util where

import Protolude

import Control.Monad.Random.Lazy
import Data.Functor.Compose

type Weight = Double

liftCR :: (Applicative f) => (f a -> f b) 
      -> Compose (Rand StdGen) f a
      -> Compose (Rand StdGen) f b
liftCR f = Compose . liftA f . getCompose

liftCR2 :: (Applicative f) => (f a -> f b -> f c)
       -> Compose (Rand StdGen) f a
       -> Compose (Rand StdGen) f b
       -> Compose (Rand StdGen) f c
liftCR2 f a b = Compose $ liftA2 f (getCompose a) (getCompose b)


