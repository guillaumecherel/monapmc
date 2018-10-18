{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Algorithm where

import Protolude
import qualified Text.Parsec as P

data Algorithm =
  Lenormand2012
    { getN :: Int
    , getAlpha :: Double
    , getPAccMin :: Double
    }
  | Beaumont2009
    { getN :: Int
    , getEpsilonFrom :: Double
    , getEpsilonTo :: Double
    }
  | SteadyState
    { getN :: Int
    , getAlpha :: Double
    , getPAccMin :: Double
    , getParallel :: Int
    }
  deriving (Show, Read, Eq, Ord)

eqAlgorithm :: Algorithm -> Algorithm -> Bool
eqAlgorithm Lenormand2012{} Lenormand2012{} = True
eqAlgorithm Beaumont2009{} Beaumont2009{} = True
eqAlgorithm SteadyState{} SteadyState{} = True
eqAlgorithm _ _ = False


pprintAlgorithm :: Algorithm -> Text
pprintAlgorithm Lenormand2012
  { getN = n
  , getAlpha = alpha
  , getPAccMin = pAccMin
  } =
  "Lenormand2012 n=" <> (show n)
            <> " alpha=" <> (show alpha)
            <> " pAccMin=" <> (show pAccMin)
pprintAlgorithm Beaumont2009
  { getN=n
  , getEpsilonTo=epsilonTo
  , getEpsilonFrom=epsilonFrom
  } =
  "Beaumont2009 n=" <> (show n) <> " epsilonFrom=" <> (show epsilonFrom) <> " epsilonTo=" <> (show epsilonTo)
pprintAlgorithm SteadyState
  { getN=n
  , getAlpha=alpha
  , getPAccMin=pAccMin
  , getParallel=parallel
  } =
  "SteadyState n=" <> (show n) <> " alpha=" <> (show alpha) <> " pAccMin=" <> (show pAccMin) <> " parallel=" <> (show parallel)

