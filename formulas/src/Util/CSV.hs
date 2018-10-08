{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Util.CSV where

import Protolude

column :: Show a => [a] -> Text
column xs = foldMap show xs

columns2 :: (Show a, Show b) => Text -> [(a, b)] -> Text
columns2 _ [] = ""
columns2 sep ((a,b):xs) = show a <> sep <> show b <> "\n" <> columns2 sep xs



