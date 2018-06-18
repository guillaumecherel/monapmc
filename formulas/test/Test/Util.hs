module Test.Util where

import Test.QuickCheck

newtype Seed = Seed Int deriving (Show)

instance Arbitrary Seed where
  arbitrary = Seed <$> choose (-100000, 100000)

