{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MonPar where

import Protolude 

import qualified Data.List.NonEmpty as NonEmpty

-- -- Run the algorithm sequentially.
-- scanSeq
--   :: forall m s. (Monad m, Semigroup s)
--   => m s
--   -> (m s -> m s)
--   -> (m s -> m Bool)
--   -> m [s]
-- scanSeq setup step stop = go setup
--   where go :: m s -> m [s]
--         go ms = ifM (stop ms)
--                     (fmap return ms)
--                     (liftM2 ((<>) . return) ms (go $ step ms))

-- Run the algorithm in parallel.
scanPlasticPar
  :: forall m s. (MonadIO m, Monoid s)
  => m (NonEmpty s)
  -> (m s -> m s)
  -> (m s -> m Bool)
  -> Int
  -> m [s]
scanPlasticPar setup step stop stepSize
  | stepSize < 1 = liftIO $ die "Error, function scanPlasticPar: stepSize be strictly positive."
  | otherwise = go (return mempty) start
  where
    step' :: m s -> m s
    step' = foldl' (.) identity (replicate stepSize step)
    stepAsync :: m s -> m (Async s)
    stepAsync x = join $ liftIO . async . return <$> step' x
    start :: m (NonEmpty (Async s))
    start = join $ liftIO . traverse (async . return) <$> setup
    waitForNext :: m (NonEmpty (Async s)) -> m (s, Maybe (NonEmpty (Async s)))
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
          die ("Error during execution of function scanPlasticPar: A running job returned an exception. " <> show e)
        Just (Right a) -> return (a, mrs)
    interrupt :: m (NonEmpty (Async s)) -> m ()
    interrupt running = join $ fmap (liftIO . traverse_ cancel) running
    go :: m s -> m (NonEmpty (Async s)) -> m [s]
    go cur running = do
      (res, left) <- waitForNext running
      let new = liftM2 (<>) cur (return res)
      ifM (stop new)
        (do
          maybe (return ()) (interrupt . return) left
          pure <$> new)
        (let running' = case left of
                           Nothing -> pure <$> stepAsync new
                           Just l -> liftM2 (<>) (return l)
                                      (fmap pure (stepAsync new))
         in liftM2 (:) new (go new running'))
      
