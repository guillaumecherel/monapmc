{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.SteadyState where

import Protolude

data SteadyStateRunner m s y = SteadyStateRunner
  { _start :: m (s, NonEmpty (Running y))
  , _step :: (s, NonEmpty (Running y)) -> m (Maybe (s, NonEmpty (Running y)))}

steadyStateRunner :: (MonadIO m) =>
                     (x -> IO y)
                     -> m (s, NonEmpty x)
                     -> ((s, y) -> m (Maybe (s, x)))
                     -> SteadyStateRunner m s y
steadyStateRunner f init update = SteadyStateRunner
  { _start = init >>= steadyStateStart f
  , _step = steadyStateStep f update
  }

run :: (Monad m) => SteadyStateRunner m s y -> m s
run ssr = do
  (s, r) <- _start ssr
  mNext <- _step ssr (s, r)
  case mNext of
    Nothing -> return s
    Just (s',r') -> run ssr{_start = return (s', r')}
    
scan :: (Monad m) => SteadyStateRunner m s y -> m [s]
scan ssr = do
  (s, r) <- _start ssr
  mNext <- _step ssr (s, r)
  case mNext of
    Nothing -> return [s]
    Just (s',r') -> fmap (s:) $ scan ssr{_start = return (s',r')}

runN :: (Monad m) => Int -> SteadyStateRunner m s y -> m s
runN n SteadyStateRunner{_start=start, _step=step} =
  if n > 0
    then do
      mNext <- start >>= step
      case mNext of
        Nothing -> fmap fst start
        Just (s, r) -> runN (n - 1) SteadyStateRunner{_start=return (s,r), _step=step}
    else
      fmap fst start

scanN :: (Monad m) => Int -> SteadyStateRunner m s y -> m [s]
scanN n ssr = if n <= 0
  then fmap (\(s, _) -> [s]) (_start ssr)
  else do
    (s, r) <- _start ssr
    mNext <- _step ssr (s,r)
    case mNext of
      Nothing -> return [s]
      Just (s', r') -> do
        next <- scanN (n - 1) ssr{_start = return (s', r')}
        return $ s : next

scanIndices :: forall m s y. (Monad m) => [Int] -> SteadyStateRunner m s y -> m [s]
scanIndices indices ssr = scanIndices' 0 indices ssr
  where scanIndices' :: Int -> [Int] -> SteadyStateRunner m s y -> m [s]
        scanIndices' _ [] _ = return []
        scanIndices' cur (i:is) ssr' =
          if cur == i
            then do
              (s,r) <- _start ssr'
              mNext <- _step ssr' (s, r)
              case mNext of
                Nothing -> return [s]
                Just (s', r') -> do
                  nextRes <- scanIndices' (cur + 1) is ssr'{_start=return (s',r')}
                  return (s:nextRes)
            else do
              mNext <- _start ssr' >>= _step ssr'
              case mNext of
                Nothing -> return []
                Just (s', r') -> scanIndices' (cur + 1) (i:is) ssr'{_start=return (s', r')}

steadyStateStart :: (MonadIO m) => (x -> IO y) -> (s, NonEmpty x) -> m (s, NonEmpty (Running y))
steadyStateStart f (s, xs) = do
  running <- traverse (mkRunning . f) xs
  return (s, running)

steadyStateStep :: (MonadIO m) => (x -> IO y) -> ((s, y) -> m (Maybe (s, x))) -> (s, NonEmpty (Running y)) -> m (Maybe (s, NonEmpty (Running y)))
steadyStateStep f update (s, running) = do
  (y, running') <- getFirstDone running
  mNext <- update (s, y)
  case mNext of
    Nothing -> return Nothing
    Just (s', x) -> do
      newRunning <- mkRunning (f x)
      return $ Just (s',
        case nonEmpty running' of
          Nothing -> pure newRunning
          Just nr -> nr <> pure newRunning)

newtype Running a = Running { getMVar :: MVar a }

mkRunning :: (MonadIO m) => IO a -> m (Running a)
mkRunning ioa = do
  v <- liftIO $ newEmptyMVar
  _ <- liftIO $ forkIO (ioa >>= putMVar v)
  return (Running v)

getRunningMaybe :: (MonadIO m) => Running a -> m (Maybe a)
getRunningMaybe (Running v) = liftIO $ tryTakeMVar v

getFirstDone :: (MonadIO m) => NonEmpty (Running a) -> m (a, [Running a])
getFirstDone (r:|rs) = do
  mba <- getRunningMaybe r
  case mba of
    Just a -> return (a, rs)
    Nothing -> getFirstDone $
      case nonEmpty rs of
        Nothing -> pure r
        Just rs' -> rs' <> pure r



