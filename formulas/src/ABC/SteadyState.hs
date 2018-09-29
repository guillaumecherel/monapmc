{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ABC.SteadyState where

import Protolude
import Unsafe (unsafeHead)

import Control.Monad.Random (MonadRandom, weighted, getRandom)
import Control.Monad.Zip (mzip)
import Data.Monoid (Last(..), (<>))
import qualified Data.IntMap as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import qualified Statistics.Quantile as SQ
import Util.SteadyState

-- The algorithm parameters
data P m = P
  { n :: Int
  , nAlpha :: Int
  , pAccMin :: Double
  , parallel :: Int
  , priorSample :: m (V.Vector Double)
  , priorDensity :: V.Vector Double -> Double
  , distanceToData :: V.Vector Double -> Double
  }

-- The algorithm's state.
data S = S
  { accepteds:: V.Vector Accepted
  , readys:: V.Vector Ready
  , pendings:: Map.IntMap Simulation
  , sigmaSquared:: LA.Herm Double
  , pAcc:: Double
  , epsilon:: Double
  , curIndexReady:: Int
  , curIndexPending:: Int
  , firstSeed:: Int
  } deriving (Show)

data Simulation = Simulation
  { getTheta:: V.Vector Double
  , getY :: V.Vector Double
  , getIndex:: Int
  , getSeed :: Seed
  } deriving (Show)

data Ready = Ready
  { getSimulation :: Simulation
  , getRho :: Double
  } deriving (Show)

data Accepted = Accepted
  { getReady :: Ready
  , getWeight :: Double
  } deriving (Show)

type Seed = Int

scan :: (MonadRandom m, MonadIO m) => P m -> ((Seed, V.Vector Double) -> IO (V.Vector Double)) -> m [S]
scan p f = start >>= go
  where go (s,r) = do
          (s', r') <- step (s, r)
          if stop s'
            then return [s', s]
            else fmap (s':) (go (s',r'))
        stop s = pAcc s < pAccMin p
        SteadyStateRunner {start = start, step = step} = runner p f
          

runner :: (MonadRandom m, MonadIO m) => P m -> ((Seed, V.Vector Double) -> IO (V.Vector Double)) -> SteadyStateRunner m S Simulation
runner p f =
  let init = initialS p >>= \s -> initialThetas p s >>= \thetas -> return (s, thetas)
      f' :: (Int, Seed, V.Vector Double) -> IO Simulation
      f' (i, seed, xs) = f (seed, xs) >>= \y -> return (Simulation xs y i seed)
  in steadyStateRunner f' init (update p)

initialS :: (MonadRandom m) => P m -> m S
initialS p = do
  firstSeed <- getRandom
  return $ S
    { accepteds = V.empty
    , readys = V.empty
    , pendings = Map.empty
    , sigmaSquared = LA.trustSym $ LA.fromLists [[0]]
    , pAcc = 1
    , epsilon = 0
    , curIndexReady = 0
    , curIndexPending = parallel p - 1
    , firstSeed = firstSeed
    }
  
initialThetas :: (Monad m) => P m -> S -> m [(Int, Seed, V.Vector Double)]
initialThetas p s = do
  let ids = [0 .. curIndexPending s]
  let seeds = [firstSeed s .. firstSeed s + curIndexPending s]
  thetas <- replicateM (parallel p) (priorSample p)
  return ((,,) <$> ids <*> seeds <*> thetas)

update :: (MonadRandom m) => P m -> (S, Simulation) -> m (S, (Int, Seed, V.Vector Double))
update p (s, current) = do
  let pendingCur = Map.insert (getIndex current) current (pendings s)

  -- Find all contiguous indices in pendingCur starting at curIndexReady and take the corresponding simulations
  let indicesPending = Map.keys pendingCur
  let indicesStatePending = curIndexReady s : indicesPending
  let contiguous (i1, i2) = i2 == i1 + 1
  let lastContiguousIndex = getLast $ foldMap (Last . Just) $ takeWhile contiguous $ zip indicesStatePending indicesPending
  let curIndexReadyNew = case lastContiguousIndex of
        Nothing -> curIndexReady s
        Just (i1, i2) -> i2
  let (readyAdd, pendingNew) = Map.split (curIndexReadyNew + 1) pendingCur
  let readyNew = readys s
              <> fmap (\sim -> Ready sim (distanceToData p (getY sim)))
                      ( V.fromList (Map.elems readyAdd))

  -- Index and seed of the next simulation to run
  let curIndexPendingNew = curIndexPending s + 1
  let seedNew = curIndexPendingNew + (firstSeed s)

  -- If not enough simulations are ready yet
  if length (accepteds s) + length (readyNew) < n p
    -- Update only the sets of ready and pending simulations
    then do
      thetaNew <- (priorSample p)
      let newS = s{ readys = readyNew
                  , pendings = pendingNew
                  , curIndexReady = curIndexReadyNew
                  , curIndexPending = curIndexPendingNew }
      return (newS, (curIndexPendingNew, seedNew, thetaNew))

    -- Else if no simulation have been accepted yet
    else if null (accepteds s)
      then do
        -- Compute epsilon
        let epsilonNew = SQ.weightedAvg (nAlpha p) (n p - 1)
                                        (fmap getRho readyNew)

        -- Filter the accepted simulations
        let acceptedNew = readyNew
              & mfilter (\r -> getRho r <= epsilonNew)
              & fmap (\r -> Accepted r 1.0)

        -- Compute the new empirical weighted variance of thetas
        let sigmaSquaredNew = LA.scale 2 $ weightedCovariance
              ( LA.fromLists $ V.toList $ fmap (V.toList . getTheta . getSimulation . getReady) acceptedNew )
              ( LA.fromList $ replicate (V.length acceptedNew) 1.0 )

        -- Compute the new pAcc
        let pAcc = 1.0

        let newS = s { accepteds = acceptedNew
                     , readys = V.empty
                     , pendings = pendingNew
                     , epsilon = epsilonNew
                     , pAcc = pAcc
                     , sigmaSquared = sigmaSquaredNew
                     , curIndexReady = curIndexReadyNew
                     , curIndexPending = curIndexPendingNew
                     }

        thetaNew <- newSample (n p) newS

        return (newS, (curIndexPendingNew, seedNew, thetaNew))

      -- Else, some simulations have been accepted before
      else do
        -- Compute the new epsilon
        let epsilonNew = SQ.weightedAvg (nAlpha p) (n p - 1)
                         $ fmap getRho (fmap getReady (accepteds s) <> readyNew)

        -- Compute the new pAcc
        let countReadyAccepted = getSum
                               $ foldMap (Sum . bool 0 1 . (<= epsilonNew))
                               $ fmap getRho readyNew
        let pAccNew = fromIntegral countReadyAccepted
                    / fromIntegral (V.length readyNew)

        -- Filter the new accepted simulations
        let acceptedKeep = mfilter ((<= epsilonNew) . getRho . getReady) (accepteds s)
        let readyNewKeep = mfilter ((<= epsilonNew) . getRho) readyNew
        let accept ready = Accepted ready
                         $ weight p s (getTheta $ getSimulation ready)
        let acceptedNew = acceptedKeep <> fmap accept readyNewKeep

        -- Compute the new empirical weighted variance of thetas
        let matrixThetas =
               LA.fromLists
             $ V.toList
             $ fmap (V.toList . getTheta . getSimulation . getReady)
             $ acceptedNew
        let vectorWeights = LA.fromList $ V.toList $ fmap getWeight acceptedNew
        let sigmaSquaredNew =
             LA.scale 2 $ weightedCovariance matrixThetas vectorWeights

        let newS = s { accepteds = acceptedNew
                     , readys = V.empty
                     , pendings = pendingNew
                     , epsilon = epsilonNew
                     , pAcc = pAccNew
                     , sigmaSquared = sigmaSquaredNew
                     , curIndexReady = curIndexReadyNew
                     , curIndexPending = curIndexPendingNew
                     }
        
        thetaNew <- newSample (n p) newS

        return (newS, (curIndexPendingNew, seedNew, thetaNew))

newSample :: (MonadRandom m) => Int -> S -> m (V.Vector Double)
newSample n s = do
  resample <- weighted $ (mzip (fmap (getTheta . getSimulation . getReady) $ accepteds s) (fmap (toRational . getWeight) $ accepteds s))
  hmSeed <- getRandom
  let thetaNew = V.fromList $ unsafeHead $ LA.toLists $ LA.gaussianSample hmSeed 1 (LA.fromList $ V.toList resample) (sigmaSquared s)
  return thetaNew

weight :: P m -> S -> V.Vector Double -> Double
weight p s theta =
  let weightSum = sum (fmap getWeight (accepteds s))
      (inverseSigmaSquared, (lnDetSigmaSquared, signDetSigmaSquared)) = LA.invlndet $ LA.unSym (sigmaSquared s)
      sqrtDet2PiSigmaSquared = (2.0 * pi) ** (fromIntegral (LA.cols $ LA.unSym $ sigmaSquared s) / 2.0) * signDetSigmaSquared * exp (lnDetSigmaSquared / 2.0)
      thetaV = LA.fromList $ V.toList theta
  in priorDensity p theta / (getSum (
        foldMap (\(wJ, thetaJ) -> Sum (
                  let thetaJV = LA.fromList $ V.toList thetaJ
                      thetaDiff = thetaV - thetaJV
                  in (wJ / weightSum)
                    * (exp $ (-0.5) * ((thetaDiff LA.<# inverseSigmaSquared) LA.<.> thetaDiff) )
                    / sqrtDet2PiSigmaSquared))
                (mzip (fmap getWeight (accepteds s)) (fmap (getTheta . getSimulation . getReady) (accepteds s)))))

weightedCovariance :: LA.Matrix Double -> LA.Vector Double -> LA.Herm Double
weightedCovariance sample weights =
  let n = LA.rows sample
      weightsSum :: Double
      weightsSum = LA.sumElements weights
      weightsSumSquared = weightsSum ** 2
      weightsSquaredSum = getSum $ foldMap (\x -> Sum (x ** 2)) $ LA.toList weights
      sampleMean :: LA.Vector Double
      sampleMean = (LA.scale (1 / weightsSum) weights) LA.<# sample
      sampleCenteredWeighted = (sample - LA.asRow sampleMean) * (LA.asColumn $ sqrt weights)
      (_, covCenteredWeighted) = LA.meanCov sampleCenteredWeighted
  in LA.scale ((fromIntegral n / weightsSum) * (weightsSumSquared / (weightsSumSquared - weightsSquaredSum))) covCenteredWeighted 

