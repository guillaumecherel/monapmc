{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Replications where

-- import Protolude

-- import Data.Functor.Compose
-- import Control.Monad.Random.Lazy
-- import Data.Text (unpack)
-- import System.FilePath ((</>))
-- 
-- data Replications a = Replications Int a 
-- 
-- run :: Replications a -> (a -> b) -> [b]
-- run (Replications n a) f = replicate n (f a)
-- 
-- runA :: (Applicative f) => Replications a -> (a -> f b) -> f [b]
-- runA (Replications n a) f = replicateM n (f a)

-- repRuns :: Replications -> Rand StdGen (IO [RunResult])
-- repRuns r =
--    fmap sequence
--    $ sequence
--    $ replicate (_nReplications r)
--    $ runResult (run (_stepMax r) (_algorithm r))
-- 
-- repSteps :: Replications -> Rand StdGen (IO [StepsResult])
-- repSteps r =
--    fmap sequence
--    $ sequence
--    $ replicate (_nReplications r)
--    $ stepsResult (steps (_stepMax r) (_algorithm r))

