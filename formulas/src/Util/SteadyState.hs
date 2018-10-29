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
  , step :: (s, [Running y]) -> m (Maybe (s, [Running y]))}

steadyStateRunner :: (MonadIO m) =>
                     (x -> IO y)
                     -> m (s, [x])
                     -> ((s, y) -> m (Maybe (s, x)))
                     -> SteadyStateRunner m s y
steadyStateRunner f init update = SteadyStateRunner
  { start = init >>= steadyStateStart f
  , step = steadyStateStep f update
  }

run :: (Monad m) => SteadyStateRunner m s y -> m s
run ssr = do
  (s, r) <- start ssr
  mNext <- step ssr (s, r)
  case mNext of
    Nothing -> return s
    Just (s',r') -> run ssr{start = return (s', r')}
    
scan :: (Monad m) => SteadyStateRunner m s y -> m [s]
scan ssr = do
  (s, r) <- start ssr
  mNext <- step ssr (s, r)
  case mNext of
    Nothing -> return [s]
    Just (s',r') -> do
      next <- scan ssr{start = return (s',r')}
      return (s:next)

runN :: (Monad m) => Int -> SteadyStateRunner m s y -> m s
runN n SteadyStateRunner{start=start, step=step} =
  if n > 0
    then do
      mNext <- start >>= step
      case mNext of
        Nothing -> fmap fst start
        Just (s, r) -> runN (n - 1) SteadyStateRunner{start=return (s,r), step=step}
    else
      fmap fst start

scanN :: (Monad m) => Int -> SteadyStateRunner m s y -> m [s]
scanN n ssr = if n <= 0
  then fmap (\(s, _) -> [s]) (start ssr)
  else do
    (s, r) <- start ssr
    mNext <- step ssr (s,r)
    case mNext of
      Nothing -> return [s]
      Just (s', r') -> do
        next <- scanN (n - 1) ssr{start = return (s', r')}
        return $ s : next

scanIndices :: forall m s y. (Monad m) => [Int] -> SteadyStateRunner m s y -> m [s]
scanIndices is ssr = scanIndices' 0 is ssr
  where scanIndices' :: Int -> [Int] -> SteadyStateRunner m s y -> m [s]
        scanIndices' _ [] _ = return []
        scanIndices' cur (i:is) ssr =
          if cur == i
            then do
              (s,r) <- start ssr
              mNext <- step ssr (s, r)
              case mNext of
                Nothing -> return [s]
                Just (s', r') -> do
                  nextRes <- scanIndices' (cur + 1) is ssr{start=return (s',r')}
                  return (s:nextRes)
            else do
              mNext <- start ssr >>= step ssr
              case mNext of
                Nothing -> return []
                Just (s', r') -> scanIndices' (cur + 1) (i:is) ssr{start=return (s', r')}

steadyStateStart :: (MonadIO m) => (x -> IO y) -> (s,[x]) -> m (s, [Running y])
steadyStateStart f (s, xs) = do
  running <- traverse (mkRunning . f) xs
  return (s, running)

steadyStateStep :: (MonadIO m) => (x -> IO y) -> ((s, y) -> m (Maybe (s, x))) -> (s, [Running y]) -> m (Maybe (s, [Running y]))
steadyStateStep f update (s, running) = do
  (y, running') <- getFirstDone running
  mNext <- update (s, y)
  case mNext of
    Nothing -> return Nothing
    Just (s', x) -> do
      newRunning <- mkRunning (f x)
      return $ Just (s', running' ++ [newRunning])

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



