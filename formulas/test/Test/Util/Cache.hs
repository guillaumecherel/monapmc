{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Util.Cache where

import Protolude

import Control.Monad.Random
import Data.Text
import Data.Text.Read
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import System.Random
import System.Directory
import Test.QuickCheck
import Test.QuickCheck.Assertions
import Test.Util

import Util.Cache

readInt :: Text -> Either Text Int
readInt = bimap pack fst . signed decimal

val :: Int -> FilePath -> Cache Int
val x p = pure x
          & cache p show readInt

addOne :: FilePath -> Cache (Int -> Int)
addOne p = pure (+ 1)

shakeGo testDir = shakeArgs shakeOptions
  { shakeReport = [testDir </> "shakeReport.html"]
  , shakeFiles = testDir </> ".shake/"
  }

-- Make sure the given file does not exist, delete if necessary.
rmFile :: FilePath -> IO ()
rmFile f = do
    System.Directory.doesFileExist f
    >>= bool (return ()) (removeFile f)

-- Make sure the given directory does not exist, delete recursively if necessary.
rmRec:: FilePath -> IO ()
rmRec dir = removePathForcibly dir

-- Test that a file exists and contains the appropriate value.
testFile :: FilePath -> Text -> IO Property
testFile path content = do
  exists <- System.Directory.doesFileExist path
  if not exists
    then return $ counterexample ("File " <> path <> " does not exist") False
    else do 
      valInFile <- readFile path
      return $ counterexample ("Cache file " <> path <> " contains "
                                <> show valInFile
                                <> " but should contain \""
                                <> unpack content
                                <> "\".")
                              (valInFile == content)

---- Single values depending on no other cached value

-- Check that when a value is computed and there is no cache file, it is created.
prop_SinValCreaCache :: Property
prop_SinValCreaCache = ioProperty $ do
  let a = val 1 "test-output/formulas/Util/Cache/SinValCreaCache/a"

  -- Make sure the directory state is clean
  rmRec "test-output/formulas/Util/Cache/SinValCreaCache"

  -- Build the cache
  shakeGo "test-output/formulas/Util/Cache/SinValCreaCache/" $ buildCache a

  -- Test if the file was written and contains the appropriate value
  testFile "test-output/formulas/Util/Cache/SinValCreaCache/a" "1"
 

-- Check that a value is not recomputed when the cache file already exists
prop_SinValNoRecomp :: Property
prop_SinValNoRecomp = ioProperty $ do
  let a x = val x "test-output/formulas/Util/Cache/SinValNoRecomp/a"

  -- Make sure the file is not present
  rmRec "test-output/formulas/Util/Cache/SinValNoRecomp"

  -- Build the cache once.
  shakeGo "test-output/formulas/Util/Cache/SinValNoRecomp" $ buildCache (a 1)

  -- Rebuild, changing the value.
  shakeGo "test-output/formulas/Util/Cache/SinValNoRecomp" $ buildCache (a 2)

  -- Test if the file was written and contains the appropriate value
  testFile "test-output/formulas/Util/Cache/SinValNoRecomp/a" "1"


-- Check that a value is recomputed when the cache file has changed.
prop_SinValRecomp :: Property
prop_SinValRecomp = ioProperty $ do
  let a x = val x "test-output/formulas/Util/Cache/SinValRecomp/a"

  -- Make sure the file is not present
  rmRec  "test-output/formulas/Util/Cache/SinValRecomp"

  -- Build the cache once.
  shakeGo "test-output/formulas/Util/Cache/SinValRecomp" $ buildCache (a 1)

  -- Modify the cache file. (delay to make sure the file modification date is different)
  threadDelay 5000
  writeFile "test-output/formulas/Util/Cache/SinValRecomp/a" "2"

  -- Rebuild, changing the value.
  shakeGo "test-output/formulas/Util/Cache/SinValRecomp/" $ buildCache (a 3)

  -- Test if the file was written and contains the appropriate value
  testFile "test-output/formulas/Util/Cache/SinValRecomp/a" "3"


---- Function application (cached values depending on another cached value)

funApTest :: FilePath -> IO () -> (Text, Text) ->  Property
funApTest dir touch (expectedA, expectedFA) = once $ ioProperty $ do
  let a x = pure x
          & cache (dir </> "a") show readInt
  let f i a = pure (+ (10 * i)) <*> a
           & cache (dir </> "fa") show readInt

  -- Delete shake cache files
  rmRec dir

  -- Build "a" once
  shakeGo dir $ do
    buildCache $ f 1 (a 1)

  touch

  -- Build cache
  shakeGo dir $ do
    buildCache $ f 2 (a 2)

  -- Test the cache files
  testA <- testFile (dir </> "a") expectedA
  testFA <- testFile (dir </> "fa") expectedFA

  return $ testA .&&. testFA



-- State: A absent, FA absent
-- Check that when no cache file is present, the function application `pure f <*> a` creates a cache file for `a` and for the last value `f <$> a`.
prop_FunApAAbsentFAAbsent :: Property
prop_FunApAAbsentFAAbsent =
  funApTest "test-output/formulas/Util/Cache/FunApAAbsentFAAbsent"
            (do
              rmFile "test-output/formulas/Util/Cache/FunApAAbsentFAAbsent/a"
              rmFile "test-output/formulas/Util/Cache/FunApAAbsentFAAbsent/fa")
            ("2", "22")

-- State: A unchanged, FA absent
-- Check that when the cache file for `a` only already exists, a is not recomputed and the cache file for `f <$> a` is created.
prop_FunApAUnchangedFAAbsent :: Property
prop_FunApAUnchangedFAAbsent =
  funApTest "test-output/formulas/Util/Cache/FunApAUnchangedFAAbsent"
            (do
              rmFile "test-output/formulas/Util/Cache/FunApAUnchangedFAAbsent/fa")
            ("1", "21")

-- State: A modified, FA absent
-- Check that a is recomputed and FA is recomputed
prop_FunApAModifiedFAAbsent :: Property
prop_FunApAModifiedFAAbsent =
  funApTest "test-output/formulas/Util/Cache/FunApAModifiedFAAbsent"
            (do
              threadDelay 5000
              writeFile "test-output/formulas/Util/Cache/FunApAModifiedFAAbsent/a" "3"
              rmFile "test-output/formulas/Util/Cache/FunApAModifiedFAAbsent/fa")
            ("2", "22")




-- State: A absent, FA unchanged
-- Check that "a" and "fa" are recomputed
prop_FunApAAbsentFAUnchanged :: Property
prop_FunApAAbsentFAUnchanged =
  funApTest "test-output/formulas/Util/Cache/FunApAAbsentFAUnchanged"
            (do
              rmFile "test-output/formulas/Util/Cache/FunApAAbsentFAUnchanged/a")
            ("2", "22")

-- State: A unchanged, FA unchanged
-- Check that nothing is recomputed
prop_FunApAUnchangedFAUnchanged :: Property
prop_FunApAUnchangedFAUnchanged =
  funApTest "test-output/formulas/Util/Cache/FunApAUnchangedFAUnchanged"
            (do
              return ())
            ("1", "11")

-- State: A modified, FA unchanged
-- Check that a is recomputed and FA is recomputed
prop_FunApAModifiedFAUnchanged :: Property
prop_FunApAModifiedFAUnchanged =
  funApTest "test-output/formulas/Util/Cache/FunApAModifiedFAUnchanged"
            (do
              threadDelay 5000
              writeFile "test-output/formulas/Util/Cache/FunApAModifiedFAUnchanged/a" "3")
            ("2", "22")




-- State: A absent, FA modified
-- Check that both are recomputed
prop_FunApAAbsentFAModified :: Property
prop_FunApAAbsentFAModified =
  funApTest "test-output/formulas/Util/Cache/FunApAAbsentFAModified"
            (do
              rmFile "test-output/formulas/Util/Cache/FunApAAbsentFAModified/a"
              threadDelay 5000
              writeFile "test-output/formulas/Util/Cache/FunApAAbsentFAModified/fa" "77")
            ("2", "22")

-- State: A unchanged, FA modified
-- Check that "fa" is recomputed but not "a" 
prop_FunApAUnchangedFAModified :: Property
prop_FunApAUnchangedFAModified =
  funApTest "test-output/formulas/Util/Cache/FunApAUnchangedFAModified"
            (do
              threadDelay 5000
              writeFile "test-output/formulas/Util/Cache/FunApAUnchangedFAModified/fa" "77")
            ("1", "21")

-- State: A modified, FA modified
-- Check that both are recomputed.
prop_FunApAModifiedFAModified :: Property
prop_FunApAModifiedFAModified =
  funApTest "test-output/formulas/Util/Cache/FunApAModifiedFAModified"
            (do
              threadDelay 5000
              writeFile "test-output/formulas/Util/Cache/FunApAModifiedFAModified/a" "3"
              threadDelay 5000
              writeFile "test-output/formulas/Util/Cache/FunApAModifiedFAModified/fa" "77")
            ("2", "22")




-- State: A absent, FA absent
-- Check with the Functor operator "f <$> a"
prop_FunApCompAllFunctor :: Property
prop_FunApCompAllFunctor = once $ ioProperty $ do
  let a = pure 1
          & cache "test-output/formulas/Util/Cache/FunApCompAllFunctor/a" show readInt
  let fa = pure (+ 1) <*> a
           & cache "test-output/formulas/Util/Cache/FunApCompAllFunctor/fa" show readInt

  -- Delete shake cache files
  rmRec "test-output/formulas/Util/Cache/FunApCompAllFunctor"

  -- Build cache
  shakeGo "test-output/formulas/Util/Cache/FunApCompAllFunctor/" $ do
    buildCache fa

  -- Test the cache files
  testA <- testFile "test-output/formulas/Util/Cache/FunApCompAllFunctor/a" "1"
  testFA <- testFile "test-output/formulas/Util/Cache/FunApCompAllFunctor/fa" "2"

  return $ testA .&&. testFA



runTests = do
  checkOrExit prop_SinValCreaCache
  checkOrExit prop_SinValNoRecomp
  checkOrExit prop_SinValRecomp
  checkOrExit prop_FunApAAbsentFAAbsent
  checkOrExit prop_FunApAAbsentFAUnchanged
  checkOrExit prop_FunApAAbsentFAModified
  checkOrExit prop_FunApAUnchangedFAAbsent
  checkOrExit prop_FunApAUnchangedFAUnchanged
  checkOrExit prop_FunApAUnchangedFAModified
  checkOrExit prop_FunApAModifiedFAAbsent
  checkOrExit prop_FunApAModifiedFAUnchanged
  checkOrExit prop_FunApAModifiedFAModified
  checkOrExit prop_FunApCompAllFunctor

