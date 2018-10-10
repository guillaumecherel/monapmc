module Test.Util where

import Protolude
import Test.QuickCheck
import Test.QuickCheck.Test

newtype Seed = Seed Int deriving (Show)

instance Arbitrary Seed where
  arbitrary = Seed <$> choose (-100000, 100000)

checkOrExit :: (Testable prop) => prop -> IO ()
checkOrExit p = do
  success <- fmap isSuccess $ quickCheckResult p
  when (not success) exitFailure
