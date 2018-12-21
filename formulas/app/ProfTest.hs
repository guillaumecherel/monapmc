{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Protolude 

import Data.List (last)
import Data.Cached
import qualified Data.Vector as V
import Control.Parallel
import Control.Monad.Random.Lazy
import Development.Shake hiding (par)


import qualified Algorithm
import Model
import L2VSNSimus
import Steps
import qualified ABC.Lenormand2012 as Lenormand2012

main :: IO ()
main = do
  ran <- getStdGen
  -- t1 <- forkThread $ do
  --         a <- Lenormand2012.scan p toyModel
  --         let t = show $ sum $ Lenormand2012.weights $ last a :: Text
  --         putStrLn t
  -- t2 <- forkThread $ do
  --         a <- Lenormand2012.scan p toyModel
  --         let t = show $ sum $ Lenormand2012.weights $ last a :: Text
  --         putStrLn t
  -- t3 <- forkThread $ do
  --         a <- Lenormand2012.scan p toyModel
  --         let t = show $ sum $ Lenormand2012.weights $ last a :: Text
  --         putStrLn t
  let v1 = len 1
  let v2 = len 2
  let v3 = len 3
  v1 `par` (v2 `par` (v3 `pseq` traverse (putStrLn . (show :: Double -> Text)) [v1, v2, v3]))
  putStrLn ("Done." :: Text)

len :: Int -> Double
len i =
  let algo = Algorithm.Lenormand2012 5000 0.4 0.1
      p = Lenormand2012.P
        { Lenormand2012.n = Algorithm.getN algo
        , Lenormand2012.nAlpha = floor $ (Algorithm.getAlpha algo) 
                                 * (fromIntegral $ Algorithm.getN algo)
        , Lenormand2012.pAccMin = Algorithm.getPAccMin algo
        , Lenormand2012.priorSample = toyPriorRandomSample
        , Lenormand2012.priorDensity = toyPrior
        , Lenormand2012.distanceToData = \x -> abs (V.head x - 0)
        }
  in  sum $ Lenormand2012.weights $ last
             $ evalRand (Lenormand2012.scan p toyModel) (mkStdGen i)

sumInt :: Int -> Double
sumInt i = sum $ last $ take 10 $ (fmap . fmap) (+ 1.0) $ repeat $ V.fromList [fromIntegral i..1e7]

forkThread :: IO () -> IO (MVar ())
forkThread proc = do
      handle <- newEmptyMVar
      _ <- forkFinally proc (\_ -> putMVar handle ())
      return handle
