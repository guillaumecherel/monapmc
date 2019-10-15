{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Steps where

import Protolude 

import Data.Text (unpack, unlines)
import Data.Csv
import Data.Functor.Compose
import Data.Cached as Cached
import Control.Monad.Random.Lazy
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import System.Random (StdGen, mkStdGen)
import System.FilePath ((</>))

import Algorithm
import Model
import qualified Run
import Statistics
import qualified ABC.Lenormand2012 as APMC
import qualified ABC.MonAPMC as MonAPMC
import qualified ABC.SteadyState as SteadyState
import Util
import Util.CSV
import Util.Figure
import qualified Util.SteadyState as SteadyState 

data Steps = Steps { _algorithm :: Algorithm
                   , _stepMax :: Int}
  deriving (Show, Read)

steps :: Int -> Algorithm -> Steps
steps = flip Steps

data StepResult = StepResult
  { _t :: Int
  , _epsilon :: Double
  , _pAcc :: Double
  , _sample :: V.Vector (Run.Weight, V.Vector Double) }
  deriving (Show, Read)

data StepsResult = StepsResult
  { _steps :: [StepResult] }
  deriving (Show, Read)

stepsResult :: Steps -> Rand StdGen (IO StepsResult)
stepsResult Steps
  { _algorithm=APMC{ getN=n
                            , getNAlpha=nAlpha
                            , getPAccMin=pAccMin}} =
  let steps' :: Rand StdGen [APMC.S]
      steps' = APMC.scan p toyModel
      p = APMC.P
        { APMC.n = n
        , APMC.nAlpha = nAlpha
        , APMC.pAccMin = pAccMin
        , APMC.priorSample = toyPriorRandomSample
        , APMC.priorDensity = toyPrior
        , APMC.observed = V.singleton 0
        }
      getStep r = StepResult
         { _t = APMC.t r
         , _epsilon = APMC.epsilon r
         , _pAcc = APMC.pAcc r
         , _sample = V.zip (V.fromList $ LA.toList $ APMC.weights r)
                        (V.fromList $ fmap V.fromList $ LA.toLists
                          $ APMC.thetas r) }
  in return . StepsResult . fmap getStep <$> steps'
stepsResult Steps
  { _algorithm=MonAPMC{ getN=n
                      , getNAlpha=nAlpha
                      , getPAccMin=pAccMin
                      , getStepSize=stepSize
                      , getParallel=parallel
                      , getStopSampleSizeFactor=sf}} =
  let steps' :: RandT StdGen IO [MonAPMC.S (RandT StdGen IO)]
      steps' = MonAPMC.scanPar stepSize parallel p toyModel
      p = MonAPMC.P
          { MonAPMC._apmcP=APMC.P
              { APMC.n = n
              , APMC.nAlpha = nAlpha
              , APMC.pAccMin = pAccMin
              , APMC.priorSample = toyPriorRandomSample
              , APMC.priorDensity = toyPrior
              , APMC.observed = V.singleton 0
              }
          , MonAPMC._stopSampleSizeFactor=sf
          }
      getStep MonAPMC.E = StepResult 0 0 0 mempty
      getStep MonAPMC.S{MonAPMC._s = s} = StepResult
        { _t = APMC.t s
        , _epsilon = APMC.epsilon s
        , _pAcc = APMC.pAcc s
        , _sample = V.zip (V.fromList $ LA.toList $ APMC.weights s)
              (V.fromList $ fmap V.fromList $ LA.toLists
                 $ APMC.thetas s)  }
  in do
    g <- getSplit
    return $ StepsResult . fmap getStep <$> evalRandT steps' g
stepsResult Steps
  { _algorithm=SteadyState{}} = return $ return $ StepsResult mempty
stepsResult Steps
  { _algorithm=Beaumont2009{}} = return $ return $ StepsResult mempty


---- Statistics over steps ----

l2Toy :: StepResult -> Double
l2Toy r = Statistics.l2Toy $ _sample r

histogramStep :: StepResult -> [(Double, Double)]
histogramStep = estPostDen (-10) 10 300 . fmap (second V.head) . V.toList . _sample

