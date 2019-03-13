{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ABC.MonAPMC where

import Protolude 

import Control.Monad.Random.Lazy
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector as V

import qualified ABC.Lenormand2012 as APMC
import qualified MonPar
import ABC.Lenormand2012 (P(..))

data S m = S {_p :: APMC.P m, _s :: APMC.S}
       | E

-- The semigroup instance holds only for the states (values of type S) of the same algorithm, i.e. for any (S p s1) and (S p s2). States are created with the function `stepOne` and iterated over with the function `step` which take as argument a value of type P.
-- instance Semigroup S where
--   -- Trier les échantillons par rho croissant, prendre les n premiers éléments
--   -- et fixer epsilon à la valeur de rho du n-ieme.
--   a <> E = a
--   E <> b = b
--   (S a) <> (S b) =
--     let size = LA.rows (APMC.thetas a)
--         collect s = zip3 (LA.toRows $ APMC.thetas s)
--                          (LA.toList $ APMC.weights s)
--                          (LA.toList $ APMC.rhos s)
--         union = collect a <> collect b
--         keep = take size $ sortOn (\(_,_,r) -> r) union
--         (newThetas', newWeights', newRhos') = unzip3 keep
--         newThetas = LA.fromRows newThetas'
--         newWeights = LA.vector newWeights'
--         newRhos = LA.vector newRhos'
--         newEpsilon = newRhos `LA.atIndex` (size - 1)
--     in S $ APMC.S { APMC.thetas = newThetas
--          , APMC.weights = newWeights
--          , APMC.rhos = newRhos
--          -- If one states have pAcc < pAccMin, it means that the algorithm
--          -- has reached the end. We want to keep this information, so
--          -- we simply set the new pAcc as the minimum pAcc of the two
--          -- states being merged.
--          , APMC.pAcc = min (APMC.pAcc a) (APMC.pAcc b)
--          , APMC.epsilon = newEpsilon
--          }
instance Semigroup (S m) where
  a <> E = a
  E <> a = a
  S p a <> S _ b = S p (APMC.stepMerge p a b)

instance Monoid (S m) where
  mempty = E

-- Create the initial step of the algorithm
setup 
  :: (Monad m)
  => P m
  -> (V.Vector Double -> m (V.Vector Double))
  -> m (S m)
setup p f = S p <$> APMC.stepOne p f

-- Run an iteration of the algorithm.
step
  :: (MonadRandom m)
  => P m
  -> (V.Vector Double -> m (V.Vector Double))
  -> m (S m)
  -> m (S m)
step p f ms = do
  s <- ms
  case s of
    E -> setup p f
    S{_s=s'} -> S p <$> APMC.stepGen p f s'

-- Stop condition
stop :: (Monad m) => P m -> m (S m) -> m Bool
stop p ms = do
  s <- ms
  case s of
    E -> return True
    S{_s=s'} -> return $ APMC.stop p s'

-- -- Sequential test for MonAPMC
-- scanSeq
--   :: forall m. (MonadRandom m)
--   => P m
--   -> (V.Vector Double -> m (V.Vector Double))
--   -> m [(S m)]
-- scanSeq p f = MonPar.scanSeq (setup p f) (step p f) (stop p)

-- Parallel Monoid APMC
scanPar
  :: forall m. (MonadRandom m, MonadIO m)
  => Int
  -> Int
  -> P m
  -> (V.Vector Double -> m (V.Vector Double))
  -> m [(S m)]
scanPar stepSize parallel p f
  | parallel < 1 = liftIO $ die "Error function scanPar: parallel argument must be strictly positive."
  | otherwise =
  MonPar.scanPlasticPar
    (NonEmpty.fromList <$> (replicateM parallel (setup p f)))
    (step p f)
    (stop p)
    stepSize
        

