{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.SteadyState where

import Protolude
import Control.Monad.Random.Lazy (MonadRandom)
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import qualified Statistics.Quantile as SQ

data SteadyStateRunner m s y = SteadyStateRunner
  { start :: m (s, [Running y])
  , step :: (s, [Running y]) -> m (s, [Running y]) }

steadyStateRunner :: (MonadIO m) =>
                     (x -> IO y)
                     -> m (s, [x])
                     -> ((s, y) -> m (s, x))
                     -> SteadyStateRunner m s y
steadyStateRunner f init update = SteadyStateRunner
  { start = init >>= steadyStateStart f
  , step = steadyStateStep f update
  }

runN :: (Monad m) => Int -> SteadyStateRunner m s y -> m s
runN n SteadyStateRunner{start=start, step=step} =
  fmap fst $ foldl' (>>=) start (replicate n step)

scanN :: (Monad m) => Int -> SteadyStateRunner m s y -> m [s]
scanN n r = if n <= 0
  then return []
  else do
    (state, running) <- start r
    next <- scanN (n - 1) r{start = (step r) (state, running)}
    return $ state : next

scanIndices :: forall m s y. (Monad m) => [Int] -> SteadyStateRunner m s y -> m [s]
scanIndices is ssr = scanIndices' 0 is ssr
  where scanIndices' :: Int -> [Int] -> SteadyStateRunner m s y -> m [s]
        scanIndices' _ [] _ = return []
        scanIndices' cur (i:is) ssr = do
          (sNew, rNew) <- foldl' (>>=) (start ssr)
                                 (replicate (i - cur) (step ssr))
          let ssrNew = ssr{start = return (sNew, rNew)}
          next <- scanIndices' i is ssrNew
          return (sNew:next)

runUntil :: (Monad m) => (s -> Bool) -> SteadyStateRunner m s y -> m s
runUntil stop r = do
  (state, running) <- start r
  if stop state
    then return state
    else runUntil stop r{start = (step r) (state, running)}
    
scanUntil :: (Monad m) => (s -> Bool) -> SteadyStateRunner m s y -> m [s]
scanUntil stop r = do
  (state, running) <- start r
  if stop state
    then return [state]
    else do
      next <- scanUntil stop r{start = (step r) (state, running)}
      return $ state : next 

steadyStateStart :: (MonadIO m) => (x -> IO y) -> (s,[x]) -> m (s, [Running y])
steadyStateStart f (s, xs) = do
  running <- traverse (mkRunning . f) xs
  return (s, running)

steadyStateStep :: (MonadIO m) => (x -> IO y) -> ((s, y) -> m (s, x)) -> (s, [Running y]) -> m (s, [Running y])
steadyStateStep f update (s, running) = do
  (y, running') <- getFirstDone running
  (s', x) <- update (s, y)
  newRunning <- mkRunning (f x)
  return (s', running' ++ [newRunning])

newtype Running a = Running { getMVar :: MVar a }

mkRunning :: (MonadIO m) => IO a -> m (Running a)
mkRunning ioa = do
  v <- liftIO $ newEmptyMVar
  liftIO $ forkIO (ioa >>= putMVar v)
  return (Running v)

getRunningMaybe :: (MonadIO m) => Running a -> m (Maybe a)
getRunningMaybe (Running v) = liftIO $ tryTakeMVar v

getFirstDone :: (MonadIO m) => [Running a] -> m (a, [Running a])
getFirstDone (r:rs) = do
  mba <- getRunningMaybe r
  case mba of
    Just a -> return (a, rs)
    Nothing -> getFirstDone (rs ++ [r])



