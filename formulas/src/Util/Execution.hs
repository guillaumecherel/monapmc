{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Util.Execution where

import Protolude 

import Control.Monad.Random.Lazy hiding (split)
import           Data.Vector (Vector)
import qualified Data.List.NonEmpty as NonEmpty

type Time = Double

-- Benchmarks: record computation time
timeIteratedMapReduce
  :: (MonadRandom m)
  => ((Vector Double -> m (Vector Double)) -> m s)
  -> ((Vector Double -> m (Vector Double)) -> s -> m s)
  -> (s -> Bool)
  -> Int
  -> [(Time, s)]
timeIteratedMapReduce = undefined

timeMonPar
  :: forall m s. (Monoid s)
  => (m s -> m (s, s))
  -> (m s -> m s)
  -> (m s -> m Bool)
  -> Int
  -> Int
  -> m [(Time, s)]
timeMonPar = undefined

-- Run the algorithm with the monoid parallel scheme.
runPlasticPar
  :: forall m s. (MonadIO m, Monoid s)
  => (m s -> m (s, s))
  -> (m s -> m s)
  -> (m s -> m Bool)
  -> Int
  -> Int
  -> m s
runPlasticPar split step stop stepSize parallel
  | stepSize < 1 = panic "Error, function MonPar.runPlasticPar: argument stepSize must be strictly positive."
  | parallel < 1 = panic "Error, function MonPar.runPlasticPar: argument parallel must be strictly positive."
  | otherwise = go (return mempty) (startRunners split fullStep (pure mempty) parallel)
  where
    go :: m s -> m (NonEmpty (Async s)) -> m s
    go curState running = do
      (res, left) <- waitForNext running
      let newState = liftM2 (<>) curState (return res)
      ifM (stop newState)
        (do
          maybe (return ()) (interrupt . return) left
          newState)
        (do
          (new1, new2) <- split newState
          let running' = case left of
                Nothing -> pure <$> fullStep (pure new2)
                Just l -> liftM2 (<>) (return l) (fmap pure (fullStep (pure new2)))
          go (pure new1) running')
    fullStep :: m s -> m (Async s)
    fullStep = stepAsync stepSize step

-- Scan the algorithm with the monoid parallel scheme.
scanPlasticPar
  :: forall m s. (MonadIO m, Monoid s)
  => (m s -> m (s, s))
  -> (m s -> m s)
  -> (m s -> m Bool)
  -> Int
  -> Int
  -> m [s]
scanPlasticPar split step stop stepSize parallel
  | stepSize < 1 = panic "Error, function MonPar.scanPlasticPar: argument stepSize must be strictly positive."
  | parallel < 1 = panic "Error, function MonPar.scanPlasticPar: argument parallel must be strictly positive."
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
  | parallel < 1 = panic "Error in MonPar.scanPlasticPar.start: parallel must be strictly positive."
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
