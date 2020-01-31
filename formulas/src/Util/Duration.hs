{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Util.Duration where

import Protolude

newtype Duration = Duration Integer
  deriving (Show, Read, Eq, Num, Ord, NFData)

fromPicoSeconds :: Integer -> Duration
fromPicoSeconds x = Duration x

fromSeconds :: Double -> Duration
fromSeconds x = Duration (round $ x * 10 ^ 12)

picoSeconds :: Duration -> Integer
picoSeconds (Duration d) = d

seconds :: Duration -> Double
seconds (Duration d) = fromIntegral d / 10^12
