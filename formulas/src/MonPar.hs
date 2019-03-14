{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module MonPar where

import Protolude 

import qualified Data.List.NonEmpty as NonEmpty

-- Run the algorithm with the monoid parallel scheme.
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
  | otherwise = go (return mempty) (start (pure mempty) parallel)
  where
    step' :: m s -> m s
    step' = foldl' (.) identity (replicate stepSize step)
    stepAsync :: m s -> m (Async s)
    stepAsync x = join $ liftIO . async . return <$> step' x
    start :: m s -> Int -> m (NonEmpty (Async s))
    start ms n
      | n < 1 = panic "Error in MonPar.scanPlasticPar.start: n must be strictly positive."
      | n == 1 = pure <$> stepAsync ms
      | otherwise = do (s1, s2) <- split ms
                       liftM2 (NonEmpty.cons) (stepAsync $ pure s1)
                         (start (pure s2) (n - 1))
    go :: m s -> m (NonEmpty (Async s)) -> m [s]
    go cur running = do
      (res, left) <- waitForNext running
      let new = liftM2 (<>) cur (return res)
      ifM (stop new)
        (do
          maybe (return ()) (interrupt . return) left
          pure <$> new)
        (do (new1, new2) <- split new
            let running' = case left of
                           Nothing -> pure <$> stepAsync (pure new2)
                           Just l -> liftM2 (<>) (return l)
                                      (fmap pure (stepAsync (pure new2)))
            (new1 :) <$> (go (pure new1) running'))

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
