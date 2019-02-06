{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ABC.MonAPMC where

import Protolude 

import qualified Control.Foldl as Fold
import Control.Monad.Random.Lazy
import Data.Cached as Cached
import Data.Functor.Compose
import Data.List (last, zip3, unzip3)
import Data.Text (Text, pack, unpack, unlines, intercalate)
import qualified Data.Vector as V
import Formatting hiding ((%))
import qualified Numeric.LinearAlgebra as LA
import System.Random (StdGen, mkStdGen)

import qualified ABC.Lenormand2012 as APMC
import ABC.Lenormand2012 (P(..))

data S = S {_s :: APMC.S}
       | E
       deriving (Show)

-- The semigroup instance holds only for the states (values of type S) of the same algorithm (type P). States are created with the function `stepOne` and iterated over with the function `step` which take as argument a value of type P.
instance Semigroup S where
  -- Trier les échantillons par rho croissant, prendre les n premiers éléments
  -- et fixer epsilon à la valeur de rho du n-ieme.
  a <> E = a
  E <> b = b
  (S a) <> (S b) =
    let size = LA.rows (APMC.thetas a)
        collect s = zip3 (LA.toRows $ APMC.thetas s)
                         (LA.toList $ APMC.weights s)
                         (LA.toList $ APMC.rhos s)
        union = collect a <> collect b
        keep = take size $ sortOn (\(_,_,r) -> r) union
        (newThetas', newWeights', newRhos') = unzip3 keep
        newThetas = LA.fromRows newThetas'
        newWeights = LA.vector newWeights'
        newRhos = LA.vector newRhos'
        newEpsilon = newRhos `LA.atIndex` (size - 1)
    in S $ APMC.S { APMC.thetas = newThetas
         , APMC.weights = newWeights
         , APMC.rhos = newRhos
         -- If one states have pAcc < pAccMin, it means that the algorithm
         -- has reached the end. We want to keep this information, so
         -- we simply set the new pAcc as the minimum pAcc of the two
         -- states being merged.
         , APMC.pAcc = min (APMC.pAcc a) (APMC.pAcc b)
         , APMC.epsilon = newEpsilon
         }

instance Monoid S where
  mempty = E

-- Create the initial step of the algorithm
setup 
  :: (MonadRandom m)
  => P m
  -> (V.Vector Double -> m (V.Vector Double))
  -> m S
setup p f = S <$> APMC.stepOne p f

-- Run an iteration of the algorithm.
step
  :: (MonadRandom m)
  => P m
  -> (V.Vector Double -> m (V.Vector Double))
  -> m S
  -> m S
step p f ms = do
  s <- ms
  case s of
    E -> return E
    S s' -> S <$> APMC.step p f s'

-- Stop condition
stop :: (MonadRandom m) => P m -> m S -> m Bool
stop p ms = do
  s <- ms
  case s of
    E -> return True
    S s' -> return $ APMC.stop p s'

-- Run the algorithm sequentially. This should be equivalent to
-- ABC.Lenormand2012.run
runSeq
  :: forall m. (MonadRandom m)
  => P m
  -> (V.Vector Double -> m (V.Vector Double))
  -> m S
runSeq p f = go (setup p f)
  where go :: m S -> m S
        go ms = ifM (stop p ms)
                    ms
                    (go (step p f ms))

scanSeq
  :: forall m. (MonadRandom m)
  => P m
  -> (V.Vector Double -> m (V.Vector Double))
  -> m [S]
scanSeq p f = go (setup p f)
  where go :: m S -> m [S]
        go ms = ifM (stop p ms)
                    (fmap return ms)
                    (liftM2 ((<>) . return) ms (go $ step p f ms))
