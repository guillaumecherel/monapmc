{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ABC.MonAPMC where

import Protolude 

import Control.Monad.Random.Lazy hiding (split)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA

import qualified ABC.Lenormand2012 as APMC
import qualified MonPar
import ABC.Lenormand2012 (P(..))

data S m = S {_p :: APMC.P m, _s :: APMC.S}
       | E

-- The semigroup instance holds only for the states (values of type S) of the same algorithm, i.e. for any (S p s1) and (S p s2). States are created with the function `stepOne` and iterated over with the function `step` which take as argument a value of type P.
instance Semigroup (S m) where
  a <> E = a
  E <> a = a
  S p a <> S _ b = S p (stepMerge p a b)

stepMerge :: P m -> APMC.S -> APMC.S -> APMC.S
stepMerge p s s' = 
  let (s1, s2) = if (APMC.t0 s <= APMC.t0 s') then (s, s') else (s', s)
      indices2NoDup = fmap fst
                     $ filter (\(_, t) -> t > APMC.t0 s2)
                     $ zip [0..] $ V.toList $ APMC.ts s2
      indicesNoDup = fmap (\i -> (1::Int,i)) [0..APMC.nAlpha p - 1]
                  <> fmap (\i -> (2::Int,i)) indices2NoDup
      selectBoth = V.fromList
                   $ take (APMC.nAlpha p)
                   $ sortOn (\(which, i) -> if which == 1
                                             then APMC.rhos s1 LA.! i
                                             else APMC.rhos s2 LA.! i)
                   $ indicesNoDup
      (select1, select2) = bimap (fmap snd) (fmap snd)
                           $ V.partition (\(w,_) -> w == 1) selectBoth
      rhosSelected = LA.vector $ V.toList (fmap (APMC.rhos s1 LA.!) select1
                                 <> fmap (APMC.rhos s2 LA.!) select2)
      tsSelected = fmap (APMC.ts s1 V.!) select1
                   <> fmap (\i -> (APMC.ts s2 V.! i) - APMC.t0 s2 + APMC.t s1)
                        select2
      newEpsilon = let (which, i) = V.last selectBoth
                   in if which == 1 then APMC.rhos s1 LA.! i
                                    else APMC.rhos s2 LA.! i
      thetasSelected = APMC.thetas s1 LA.? V.toList select1
                       LA.=== APMC.thetas s2 LA.? V.toList select2
      weightsSelected = LA.fromList $ V.toList
                        ( fmap (APMC.weights s1 LA.!) select1
                          <> fmap (APMC.weights s2 LA.!) select2 )
  in  APMC.S { APMC.t0 = APMC.t0 s1
             , APMC.t = APMC.t s2
             , APMC.ts = tsSelected
             , APMC.thetas = thetasSelected
             , APMC.weights = weightsSelected
             , APMC.rhos = rhosSelected
             , APMC.pAcc = APMC.pAcc s2
             , APMC.epsilon = newEpsilon
             }


instance Monoid (S m) where
  mempty = E

-- Split the algorithm state
split :: (MonadRandom m) => P m -> m (S m) -> m (S m, S m)
split p ms = do
  s <- ms
  return $ case s of
    E -> (E, E)
    S{_s = s'} -> (s, s{_s = s'{APMC.t0 = APMC.t s'}})


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
    E -> S p <$> APMC.stepOne p f
    S{_s=s'} -> S p <$> APMC.step p f s'

-- Stop condition
stop :: (MonadRandom m) => P m -> m (S m) -> m Bool
stop p ms = do
  s <- ms
  case s of
    E -> return True
    S{_s=s'} -> return $ APMC.stop p s'

-- Parallel Monoid APMC
scanPar
  :: forall m. (MonadIO m, MonadRandom m)
  => Int
  -> Int
  -> P m
  -> (V.Vector Double -> m (V.Vector Double))
  -> m [(S m)]
scanPar stepSize parallel p f
  | parallel < 1 = panic "Error function MonAPMC.scanPar: parallel argument must be strictly positive."
  | otherwise =
  MonPar.scanPlasticPar (split p) (step p f) (stop p) stepSize parallel 

