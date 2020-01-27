{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module ABC.MonAPMC where

import Protolude 

import Control.Monad.Random.Lazy hiding (split)
-- import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA

import qualified ABC.Lenormand2012 as APMC
import qualified Util.Execution as Execution

data P m = P {_apmcP :: APMC.P m,
              _stopSampleSize :: Int}

instance NFData (P m) where
  rnf (P p sss) = rnf p `seq` rnf sss

data S m = S {_p :: !(P m), _t0 :: !Int, _s :: !(APMC.S)}
         | E

instance NFData (S m) where
  rnf (S p t0 s) = rnf p `seq` rnf t0 `seq` rnf s
  rnf E = ()

-- The semigroup instance holds only for the states (values of type S) of the same algorithm, i.e. for any (S p s1) and (S p s2). States are created with the function `stepOne` and iterated over with the function `step` which take as argument a value of type P.
instance Semigroup (S m) where
  a <> E = a
  E <> a = a
  s <> s' = stepMerge s s'

stepMerge :: S m -> S m -> S m
stepMerge s s' =
  let (s1, s2) = if (_t0 s <= _t0 s') then (s, s') else (s', s)
      p = _p s1
      -- Select particles from both states filtering out those from s2 that are
      -- duplicates from particles in s1, i.e. those which creation predate
      -- s2's creation time.
      indices2NoDup = fmap fst
                     $ filter (\(_, t) -> t > _t0 s2)
                     $ zip [0..] $ V.toList $ APMC.ts (_s s2)
      indicesNoDup = fmap (\i -> (1::Int,i))
                       [0..LA.rows (APMC.thetas (_s s1)) - 1]
                  <> fmap (\i -> (2::Int,i)) indices2NoDup
      -- While filtering the particles, sort them by their rho values and keep
      -- only those with lower rho.
      selectBoth = V.fromList
                   $ take (APMC.nAlpha $ _apmcP p)
                   $ sortOn (\(which, i) -> if which == 1
                                             then APMC.rhos (_s s1) LA.! i
                                             else APMC.rhos (_s s2) LA.! i)
                   $ indicesNoDup
      -- Split particle indices belonging to s1 and s2.
      (select1, select2) = bimap (fmap snd) (fmap snd)
                           $ V.partition (\(w,_) -> w == 1) selectBoth
      -- Compute the values for the new algorithm state from the selected
      -- particles.
      -- TODO: le calcule du quantile devrait être pondéré? (idem dans APMC.hs)
      rhosSelected = LA.vector $ V.toList
        (fmap (APMC.rhos (_s s1) LA.!) select1 <>
         fmap (APMC.rhos (_s s2) LA.!) select2)
      tsSelected =
        fmap (APMC.ts (_s s1) V.!) select1 <>
        fmap (\i -> (APMC.ts (_s s2) V.! i) - _t0 s2 + APMC.t (_s s1))
          select2
      newEpsilon = let (which, i) = V.last selectBoth
                   in if which == 1 then APMC.rhos (_s s1) LA.! i
                                    else APMC.rhos (_s s2) LA.! i
      thetasSelected = APMC.thetas (_s s1) LA.? V.toList select1 LA.===
                       APMC.thetas (_s s2) LA.? V.toList select2
      weightsSelected = LA.fromList $ V.toList
        (fmap (APMC.weights (_s s1) LA.!) select1 <>
         fmap (APMC.weights (_s s2) LA.!) select2 )
  in  S { _p = _p s1
        , _t0 = _t0 s1
        , _s = APMC.S { APMC.t = APMC.t (_s s1) + APMC.t (_s s2) - _t0 s2
                      , APMC.ts = tsSelected
                      , APMC.thetas = thetasSelected
                      , APMC.weights = weightsSelected
                      , APMC.rhos = rhosSelected
                      , APMC.pAcc = (fromIntegral $ V.length select2) *
                                    (APMC.pAcc $ _s s2) /
                                    (fromIntegral $ LA.rows $ APMC.thetas $
                                      _s s2)
                      , APMC.epsilon = newEpsilon
                      }}

instance Monoid (S m) where
  mempty = E

-- Split the algorithm state
split :: (MonadRandom m) => m (S m) -> m (S m, S m)
split ms = do
  s <- ms
  return $ case s of
    E -> (E, E)
    S{_s = s'} -> (s, s{_t0 = APMC.t s'})


-- Run an iteration of the algorithm.
step
  :: (MonadRandom m)
  => P m
  -> (V.Vector Double -> m (V.Vector Double))
  -> m (S m)
  -> m (S m)
step p f ms = do
  s <- ms
  -- If s is empty or the number of particles it contains hasn't reach nAlpha
  -- yet, keep generating particles (n - nAlpha by n - nAlpha) from the prior
  -- using the function APMC.stepOne and setting the parameter n to
  -- (n - nAlpha)
  let reducedN = (_apmcP p){APMC.n = APMC.n (_apmcP p) - APMC.nAlpha (_apmcP p)}
  case s of
    E -> (\s' -> S { _p = p, _t0 = 0, _s = s'}) <$> APMC.stepOne reducedN f
    S{_s=s'} ->
      if LA.rows (APMC.thetas s') < APMC.nAlpha (_apmcP p)
        then (\s'' ->  s <> S { _p = p, _t0 = 0, _s = s''}) <$> APMC.stepOne reducedN f
        else (\s'' -> s{_s = s''}) <$> APMC.step (_apmcP p) f s'

-- Stop condition
stop :: (MonadRandom m) => P m -> m (S m) -> m Bool
stop P{_apmcP=apmcP, _stopSampleSize=sss} ms = do
  s <- ms
  case s of
    E -> return False
    S{_s=s'} ->
      let tSpan :: Int
          tSpan = ceiling (fromIntegral sss /
                   fromIntegral (APMC.n apmcP - APMC.nAlpha apmcP) :: Double)
          count :: Int
          count = getSum $ foldMap
                             (\t -> if t > APMC.t s' - tSpan then Sum 1 else Sum 0)
                             (APMC.ts s')
          pAcc :: Double
          pAcc = (fromIntegral count) / (fromIntegral (tSpan * (APMC.n apmcP - APMC.nAlpha apmcP)))
      in return (APMC.t s' >= tSpan &&
                  (APMC.pAccMin apmcP :: Double) >= (pAcc :: Double) )

-- Parallel Monoid APMC
-- runPar
--   :: forall m. (MonadIO m, MonadRandom m)
--   => Int
--   -> Int
--   -> P m
--   -> (V.Vector Double -> m (V.Vector Double))
--   -> m (S m)
-- runPar stepSize parallel p f
--   | parallel < 1 = panic "Error function MonAPMC.runPar: parallel argument must be strictly positive."
--   | otherwise =
--   Execution.runPlasticPar split (step p f) (stop p) stepSize parallel

scanPar
  :: Int
  -> Int
  -> P (Rand StdGen)
  -> (V.Vector Double -> Rand StdGen (V.Vector Double))
  -> RandT StdGen IO [S (Rand StdGen)]
scanPar stepSize parallel p f
  | parallel < 1 = panic "Error function MonAPMC.scanPar: parallel argument must be strictly positive."
  | otherwise =
  (fmap . fmap) snd
  $ Execution.simPlasticPar
      split
      ((0,) <<$>> step p f)
      (stop p)
      stepSize
      parallel
      mempty

