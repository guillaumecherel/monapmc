{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Util.Execution where

import Protolude 

import Control.Exception (evaluate)
import Control.DeepSeq (force)
import Control.Monad.Random.Lazy hiding (split)
import Control.Monad.Morph (hoist, generalize)
import           Data.Vector (Vector)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import System.CPUTime (getCPUTime)
import           Util (generalizeRand)

type Duration = Integer -- picoseconds

-- Benchmarks: record computation time
timeIteratedMapReduce
  :: (MonadRandom m)
  => ((Vector Double -> m (Vector Double)) -> m s)
  -> ((Vector Double -> m (Vector Double)) -> s -> m s)
  -> (s -> Bool)
  -> Int
  -> [(Duration, s)]
timeIteratedMapReduce = undefined

timeMonPar
  :: forall m s. (Monoid s)
  => (m s -> m (s, s))
  -> (m s -> m s)
  -> (m s -> m Bool)
  -> Int
  -> Int
  -> m [(Duration, s)]
timeMonPar = undefined

-- Simulate running the argorithm with the naive parallelisation scheme.
simEasyPar
  :: forall s z x y. (NFData s)
  => (s -> Rand StdGen (z,[x]))
  -> (x -> Rand StdGen (Duration,y))
  -> (s -> (z, [x]) -> [y] -> Rand StdGen s)
  -> (s -> Bool)
  -> Int
  -> s
  -> RandT StdGen IO [(Duration, s)]
simEasyPar !stepPre !f !stepPost !stop !parallel !init
  = do
      start <- liftIO getCPUTime
      go init start 0
  where
    go :: s -> Integer -> Duration -> RandT StdGen IO [(Duration, s)]
    go !s !startTime !accSimTime =
      if stop s
        then return []
        else do
          -- Use evaluate and force to make sure computation is done before the
          -- next call to getCPUTime.
          stepRes <- liftRandT $ fmap return $ runRand $ step s
          (simElapsed, s') <- liftIO $ evaluate $ force stepRes
          now <- liftIO getCPUTime
          fmap ((accSimTime + simElapsed + now - startTime, s'):) $ go s' startTime (accSimTime + simElapsed)
    step :: s -> Rand StdGen (Duration, s)
    step s = do
      (z, xs) <- stepPre s
      (simElapsed, ys) <- simScheduler xs
      s' <- stepPost s (z, xs) ys
      return (simElapsed, s')
    -- Preserve order of input and output!
    simScheduler :: [x] -> Rand StdGen (Duration, [y])
    simScheduler xs = do
      ys <- traverse f xs
      let durationsSorted = List.sort $ fmap fst ys
      return (simRunTimeSorted durationsSorted, fmap snd ys)
    simRunTimeSorted :: [Duration] -> Duration
    simRunTimeSorted [] = 0
    simRunTimeSorted (shortestDuration:rest) =
      let (running, waiting) = List.splitAt (parallel - 1) rest
          duration = simRunTimeSorted
               ( fmap (subtract shortestDuration) running
              ++ waiting
               )
      in  duration + shortestDuration


simPlasticPar
  :: forall s. (Monoid s, NFData s)
  => (Rand StdGen s -> Rand StdGen (s, s))
  -> (Rand StdGen s -> Rand StdGen (Duration, s))
  -> (Rand StdGen s -> Rand StdGen Bool)
  -> Int
  -> Int
  -> s
  -> RandT StdGen IO [(Duration, s)]
simPlasticPar !split !step !stop !stepSize !parallel !init
  | stepSize < 1 = panic "Error, function Execution.simPlasticPar: argument stepSize must be strictly positive."
  | parallel < 1 = panic "Error, function Execution.simPlasticPar: argument parallel must be strictly positive."
  | otherwise = do
      startTime <- liftIO getCPUTime
      (startState, runners) <-
        generalizeRand $ simStartRunnersSorted init parallel
      go startTime 0 startState runners
  where
    go :: Integer -> Duration -> s -> (NonEmpty (Duration, s)) -> RandT StdGen IO [(Duration, s)]
    go !startTime !accSimTime !curState !running =
      ifM (generalizeRand $ stop $ return curState)
        (do
          (accSimTimeNF, curStateNF) <-
            liftIO $ evaluate $ force (accSimTime, curState)
          now <- liftIO getCPUTime
          return [(accSimTimeNF + now - startTime, curStateNF)])
        (do
          let (simElapsed, (res, left)) = simWaitForNextSorted running
          let newState = curState <> res
          (new1, new2) <- generalizeRand $ split (pure newState)
          newRunning <- generalizeRand $ case left of
                Nothing -> pure <$> simFullStep new2
                Just l -> liftM2 insertSorted
                  (simFullStep new2)
                  (return l)
          -- Make sure all computation is performed before calling getCPUTime
          new1NF <- liftIO $ evaluate $ force new1
          newRunningNF <- liftIO $ evaluate $ force newRunning
          now <- liftIO getCPUTime
          ((accSimTime + simElapsed + now - startTime, new1NF) :)
            <$> (go startTime (accSimTime + simElapsed) new1NF newRunningNF))
    simStartRunnersSorted
      :: s -> Int -> Rand StdGen (s, NonEmpty (Duration, s))
    simStartRunnersSorted s n = do
      (s1, s2) <- split (pure s)
      if n == 1
        then
          (s1,) . pure <$> simFullStep s2
        else do
          newRunning <- simFullStep s2
          (sFinal, otherRunning) <- simStartRunnersSorted s1 (n - 1)
          return $ (sFinal, insertSorted newRunning otherRunning)
    simWaitForNextSorted
      :: (NonEmpty (Duration, s))
      -> (Duration, (s, Maybe (NonEmpty (Duration, s))))
    simWaitForNextSorted running =
      let ((duration, s), rest) = NonEmpty.uncons running
          rest' = (fmap . fmap . first) (subtract duration) rest
      in (duration, (s, rest'))
    simFullStep :: s -> Rand StdGen (Duration, s)
    simFullStep curState = simFullStepGo stepSize 0 curState
    simFullStepGo :: Int -> Integer -> s -> Rand StdGen (Duration, s)
    simFullStepGo n accDur curState =
      if n <= 0
        then return (accDur, curState)
        else do
          (dur, newState) <- step $ pure curState
          simFullStepGo (n - 1) (accDur + dur) newState
    insertSorted
      :: (Duration, s) -> NonEmpty (Duration, s) -> NonEmpty (Duration, s)
    insertSorted ds dss =
      NonEmpty.fromList
      $ List.insertBy (comparing fst) ds
      $ NonEmpty.toList dss



-- !TO BE REMOVED!
-- Run the algorithm with the monoid parallel scheme.
-- runPlasticPar
--   :: forall m s. (MonadIO m, Monoid s)
--   => (m s -> m (s, s))
--   -> (m s -> m s)
--   -> (m s -> m Bool)
--   -> Int
--   -> Int
--   -> m s
-- runPlasticPar split step stop stepSize parallel
--   | stepSize < 1 = panic "Error, function Execution.runPlasticPar: argument stepSize must be strictly positive."
--   | parallel < 1 = panic "Error, function Execution.runPlasticPar: argument parallel must be strictly positive."
--   | otherwise = go (return mempty) (startRunners split fullStep (pure mempty) parallel)
--   where
--     go :: m s -> m (NonEmpty (Async s)) -> m s
--     go curState running = do
--       (res, left) <- waitForNext running
--       let newState = liftM2 (<>) curState (return res)
--       ifM (stop newState)
--         (do
--           maybe (return ()) (interrupt . return) left
--           newState)
--         (do
--           (new1, new2) <- split newState
--           let running' = case left of
--                 Nothing -> pure <$> fullStep (pure new2)
--                 Just l -> liftM2 (<>) (return l) (fmap pure (fullStep (pure new2)))
--           go (pure new1) running')
--     fullStep :: m s -> m (Async s)
--     fullStep = stepAsync stepSize step

-- Scan the algorithm with the monoid parallel scheme.
-- ! The following function isn't used for the moment, it has diverged from the
-- ! algorithm implemented in simPlasticPar and needs tu be updated.
scanPlasticPar
  :: forall m s. (MonadIO m, Monoid s)
  => (m s -> m (s, s))
  -> (m s -> m s)
  -> (m s -> m Bool)
  -> Int
  -> Int
  -> m [s]
scanPlasticPar split step stop stepSize parallel
  | stepSize < 1 = panic "Error, function Execution.scanPlasticPar: argument stepSize must be strictly positive."
  | parallel < 1 = panic "Error, function Execution.scanPlasticPar: argument parallel must be strictly positive."
  | otherwise = go (return mempty) (startRunners split fullStep (pure mempty) parallel)
  where
    go :: m s -> m (NonEmpty (Async s)) -> m [s]
    go curState running = do
      (res, left) <- waitForNext running
      let newState = liftM2 (<>) curState (return res)
      ifM (stop newState)
        (do
          maybe (return ()) (interrupt . return) left
          pure <$> newState)
        (do
          (new1, new2) <- split newState
          let running' = case left of
                Nothing -> pure <$> fullStep (pure new2)
                Just l -> liftM2 (<>) (return l)
                           (fmap pure (fullStep (pure new2)))
          (new1 :) <$> (go (pure new1) running'))
    fullStep :: m s -> m (Async s)
    fullStep = stepAsync stepSize step


stepAsync :: forall m s. (MonadIO m) => Int -> (m s -> m s) -> m s -> m (Async s)
stepAsync stepSize step x = join $ liftIO . async . return <$> step' x
  where
    step' :: m s -> m s
    step' = foldl' (.) identity (replicate stepSize step)

startRunners
  :: forall m s. (MonadIO m)
  => (m s -> m (s, s))
  -> (m s -> m (Async s))
  -> m s
  -> Int
  -> m (NonEmpty (Async s))
startRunners split fullStep ms parallel
  | parallel < 1 = panic "Error in Execution.scanPlasticPar.start: parallel must be strictly positive."
  | parallel == 1 = pure <$> fullStep ms -- pure <$> (join $ liftIO . async . return <$> fullStep ms)
  | otherwise = do (s1, s2) <- split ms
                   liftM2 (NonEmpty.cons) (fullStep $ return s1 :: m (Async s))
                     (startRunners split fullStep (pure s2) (parallel - 1))

waitForNext :: (MonadIO m) => m (NonEmpty (Async s)) -> m (s, Maybe (NonEmpty (Async s)))
waitForNext running = do
  (r, mrs) <- NonEmpty.uncons <$> running
  r' <- liftIO $ poll r
  case r' of
    Nothing -> waitForNext $ return
      (case mrs of
        Just rs -> rs <> (pure r)
        Nothing -> pure r)
    Just (Left e) -> liftIO $ do
      _ <- (traverse . traverse) cancel mrs
      throwIO e
    Just (Right a) -> return (a, mrs)

interrupt :: (MonadIO m) => m (NonEmpty (Async s)) -> m ()
interrupt running = join $ fmap (liftIO . traverse_ cancel) running
