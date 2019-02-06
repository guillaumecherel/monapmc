{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module L2VSNSimus where

import Protolude 

import qualified Control.Foldl as Fold
import Control.Monad.Random.Lazy
import Data.Cached as Cached
import Data.Functor.Compose
import Data.List (last)
import Data.Text (Text, pack, unpack, unlines, intercalate)
import qualified Data.Vector as V
import Formatting hiding ((%))
import System.Random (StdGen, mkStdGen)

import Algorithm
import Figure
import Model
import Replications
import Run
import Statistics
import qualified ABC.Lenormand2012 as Lenormand2012
import qualified ABC.SteadyState as SteadyState
import qualified Util.SteadyState as SteadyState

data L2VSNSimus = L2VSNSimus
  { _l2Mean :: Double
  , _l2Std :: Double
  , _nSimusMean :: Double
  , _nSimusStd :: Double
  }

l2VSNSimus :: Replications -> L2VSNSimus
l2VSNSimus (Replications reps) = Fold.fold f reps
  where f = L2VSNSimus <$> l2Mean <*> l2Std <*> nSimMean <*> nSimStd
        l2Mean = foldMeanWith l2Toy
        l2Std = foldStdWith l2Toy
        nSimMean = foldMeanWith (fromIntegral . nSimus)
        nSimStd = foldStdWith (fromIntegral . nSimus)

l2VSNSimus' :: Int -> Int -> Algorithm
            -> Compose (Rand StdGen) Cached L2VSNSimus
l2VSNSimus' nRep stepMax algo =
  l2VSNSimus <$> cachedReplications nRep stepMax algo

fig :: Compose (Rand StdGen) Cached ()
fig = 
  let replications = 10 -- 10
      alphas = [1%10, 2%10..9%10] -- [0.1,0.2..0.9]
      nAlpha = 5000
      pAccMins = [0.01] -- [0.01, 0.05, 0.1, 0.2]
      stepMax = 100
      lenPath = "output/formulas/l2VSNSimus/lenormand2012.csv"
      stePath = "output/formulas/l2VSNSimus/steadyState.csv"
      len pAccMin alpha = Lenormand2012
                            { getN = floor (nAlpha / alpha)
                            , getAlpha=fromRational alpha
                            , getPAccMin=pAccMin}
      ste pAccMin alpha = SteadyState
                            { getN = floor (nAlpha / alpha)
                            , getAlpha = fromRational alpha
                            , getPAccMin = pAccMin
                            , getParallel = 1}
      algos al = fmap (\pAccMin -> fmap (al pAccMin) alphas) pAccMins
      gnuplotData al =
        fmap (gnuplotData4 _nSimusMean _l2Mean _nSimusStd _l2Std)
        $ (traverse . traverse) (l2VSNSimus' replications stepMax)
        $ algos al
      gnuplotInputFile path al =
        liftC (gnuplotDataSink path)
        $ gnuplotData al
      fig :: Cached ()
      fig = gnuplot "report/L2_vs_nsimus.png" "report/L2_vs_nsimus.gnuplot"
                        [("lenormand2012", lenPath)
                        -- DEBUG ,("steadyState", stePath)]
                        ,("steadyState", lenPath)]

  in foldr (liftC2 (<>))  (Compose (pure fig))
                          [ gnuplotInputFile lenPath len
                          -- DEBUG , gnuplotInputFile stePath ste ]
                          ]

liftC :: (Cached a -> Cached b) 
      -> Compose (Rand StdGen) Cached a
      -> Compose (Rand StdGen) Cached b
liftC f = Compose . liftA f . getCompose

liftC2 :: (Cached a -> Cached b -> Cached c)
       -> Compose (Rand StdGen) Cached a
       -> Compose (Rand StdGen) Cached b
       -> Compose (Rand StdGen) Cached c
liftC2 f a b = Compose $ liftA2 f (getCompose a) (getCompose b)

buildL2VSNSimus :: Rand StdGen (Cached ())
buildL2VSNSimus = getCompose fig

