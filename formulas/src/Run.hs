{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}


module Run where

import Protolude
import Data.List (last)
import Data.Functor.Compose
import qualified Control.Foldl as L
import Control.Monad.Random.Lazy
import Data.Cached
import qualified Data.Map as Map
import Data.Text (unpack)
import qualified Data.Vector as V
import Formatting
import qualified Numeric.LinearAlgebra as LA
import qualified Text.Parsec as P
import System.FilePath ((</>))
import Util.Parser

import Algorithm
import Model
import Statistics
import qualified ABC.Lenormand2012 as Lenormand2012
import qualified ABC.MonAPMC as MonAPMC
import qualified ABC.SteadyState as SteadyState
import qualified Util.SteadyState as SteadyState 

type Weight = Double

data Run = Run { _algorithm :: Algorithm
               , _stepMax :: Int }
  deriving (Show, Read, Eq, Ord)

data RunResult = RunResult
  { _stepCount :: Int
  , _sample :: V.Vector (Weight, V.Vector Double)}
  deriving (Show, Read)

run :: Int -> Algorithm -> Run
run stepMax algo = Run algo stepMax

runResult :: Run -> Rand StdGen (IO RunResult)
runResult Run
  { _stepMax=stepMax
  , _algorithm=Lenormand2012{ getN=n
                            , getAlpha=alpha
                            , getPAccMin=pAccMin}} =
  let steps :: Rand StdGen [(Int, Lenormand2012.S)]
      steps = zip [1..stepMax] <$> Lenormand2012.scan p toyModel 
      p = Lenormand2012.P
        { Lenormand2012.n = n
        , Lenormand2012.nAlpha = floor $ alpha * (fromIntegral n)
        , Lenormand2012.pAccMin = pAccMin
        , Lenormand2012.priorSample = toyPriorRandomSample
        , Lenormand2012.priorDensity = toyPrior
        , Lenormand2012.observed = V.singleton 0
        }
      getRun (i, r) = RunResult
        { _stepCount = i
        , _sample = V.zip (V.fromList $ LA.toList $ Lenormand2012.weights r)
                          (V.fromList $ fmap V.fromList $ LA.toLists
                            $ Lenormand2012.thetas r) }
  in return . getRun . last <$> steps
runResult Run
  { _stepMax=stepMax
  , _algorithm=MonAPMC{ getN=n
                           , getAlpha=alpha
                           , getPAccMin=pAccMin
                           , getStepSize=stepSize
                           , getParallel=parallel}} =
  let steps :: RandT StdGen IO [(Int, MonAPMC.S (RandT StdGen IO))]
      steps = zip [1..stepMax] <$> MonAPMC.scanPar stepSize parallel p toyModel
      p = Lenormand2012.P
        { Lenormand2012.n = n
        , Lenormand2012.nAlpha = floor $ alpha
                                 * (fromIntegral $ n)
        , Lenormand2012.pAccMin = pAccMin
        , Lenormand2012.priorSample = toyPriorRandomSample
        , Lenormand2012.priorDensity = toyPrior
        , Lenormand2012.observed = V.singleton 0
        }
      getRun (i, (MonAPMC.E)) = RunResult i mempty
      getRun (i, (MonAPMC.S _ s)) = RunResult
        { _stepCount = i
        , _sample = V.zip (V.fromList $ LA.toList $ Lenormand2012.weights s)
                          (V.fromList $ fmap V.fromList $ LA.toLists
                            $ Lenormand2012.thetas s) }
  in do
    g <- getSplit
    return $ getRun . last <$> evalRandT steps g
runResult Run {_stepMax=stepMax, _algorithm=SteadyState{getN=n, getAlpha=alpha, getPAccMin=pAccMin, getParallel=par}} =
  let step :: RandT StdGen IO SteadyState.S
      step = SteadyState.runN (stepMax * n) ssr
      ssr = SteadyState.runner p model
      model (seed, xs) = return $ evalRand (toyModel xs) (mkStdGen seed)
      p :: SteadyState.P (RandT StdGen IO)
      p = SteadyState.P
        { SteadyState.n = n
        , SteadyState.nAlpha = floor $ alpha * (fromIntegral $ n)
        , SteadyState.pAccMin = pAccMin
        , SteadyState.parallel = par
        , SteadyState.priorSample = toyPriorRandomSample
        , SteadyState.priorDensity = toyPrior
        , SteadyState.distanceToData = absoluteError 0 . V.head
        }
      getRun r = RunResult
        { _stepCount = SteadyState.curStep r
        , _sample = fmap (\a -> (SteadyState.getWeight a, SteadyState.getTheta $ SteadyState.getSimulation $ SteadyState.getReady a)) (SteadyState.accepteds r)
        }
  in do
        g <- getSplit
        return $ fmap getRun $ evalRandT step g
runResult Run {_algorithm=Beaumont2009{}} = return (return (RunResult 0 mempty))

absoluteError :: Double -> Double -> Double
absoluteError expected x = abs (x - expected)

cachedRun :: FilePath -> Int -> Algorithm
          -> Compose (Rand StdGen) Cached RunResult
cachedRun rootDir stepMax algo =
  let r = Run algo stepMax
  in Compose $ (cache' (rootDir </> cachedRunPath r) . fromIO mempty)
           <$> (runResult r)

cachedRunPath :: Run -> FilePath
cachedRunPath Run {_stepMax = stepMax, _algorithm = algo} =
  (unpack $ "run_" <> show stepMax) </> algoFilename algo



--------
-- Descriptive statistics and quantities over a single simulation result
--------
 
nSimus :: Run -> RunResult -> Int
nSimus Run {_algorithm=Lenormand2012 {getN=n, getAlpha=alpha}}
       RunResult {_stepCount=step} =
  numberSimusLenormand2012 n (floor $ fromIntegral n * alpha) step
-- nSimus Run {_algorithm=MonAPMCSeq {getN=n, getAlpha=alpha}, _stepCount=step} = numberSimusLenormand2012 n (floor $ fromIntegral n * alpha) step
nSimus Run {_algorithm=MonAPMC {getN=n, getStepSize=stepSize, getAlpha=alpha}}
       RunResult {_stepCount=step} =
  numberSimusLenormand2012 n (floor $ fromIntegral n * alpha) (step * stepSize)
nSimus Run {_algorithm=SteadyState{}}
       RunResult {_stepCount=step} = step
nSimus Run {_algorithm=Beaumont2009{getN=n}}
       RunResult {_stepCount=step} = n * step

numberSimusLenormand2012 :: Int -> Int -> Int -> Int
numberSimusLenormand2012 n nAlpha step = n + (n - nAlpha) * step

l2 :: Double -> Double -> Int -> (Double -> Double) -> RunResult -> Double
l2 lowerBound upperBound bins cdf r = posteriorL2 lowerBound upperBound bins cdf sample
  where sample = fmap (second V.head) $ V.toList $ _sample r

l2Toy :: RunResult -> Double
l2Toy r = posteriorL2 (-10) 10 300 (toyPosteriorCDF 0)
            (V.toList $ second V.head <$> _sample r)




--------
-- Group
--------

groupReplications :: L.Fold Run r -> L.Fold Run (Map.Map Algorithm r)
groupReplications = L.groupBy _algorithm




--------
-- Parsing
--------

read1DSample :: FilePath -> Text -> Either P.ParseError (V.Vector (Weight, V.Vector Double))
read1DSample = --mapM ((fmap fst) . TR.double) (T.lines text)
  P.parse parser1DSample

parser1DSample :: (P.Stream s m Char) => P.ParsecT s u m (V.Vector (Weight, V.Vector Double))
parser1DSample = V.fromList 
             <$> (P.many $ P.try weightAndValue <* P.optional P.endOfLine)
             <*  P.eof
  where weightAndValue = (\w x -> (w, V.singleton x)) <$> parserDouble <*> parserDouble
 
-- parserSample :: (P.Stream s m Char) => P.ParsecT s u m (V.Vector (Weight, V.Vector Double))
-- parserSample = (V.fromList . fmap V.fromList)
--            <$> (P.many $ P.try (P.sepBy1 parserDouble (P.char ' '))
--                       <* P.optional P.endOfLine)
--            <*  P.eof

-- loadSimulation :: FilePath -> IO (Either P.ParseError Run)
-- loadSimulation f = (TIO.readFile f) >>= return . readRun f
-- 
-- loadAllSimulations :: FilePath -> IO [Run]
-- loadAllSimulations dir = do
--   fs <- listDirectory dir
--   ess <- traverse loadSimulation $ map (dir ++ ) fs
--   return $ foldMap toList ess

-- readRun :: FilePath -> Text -> Either P.ParseError Run
-- readRun filename column =
--   P.parse parseSimulationFileName ("filename " ++ filename) filename
--   <*> read1DSample filename column
--   where parseSimulationFileName = P.try parseLenormand2012 <|> P.try parseBeaumont2009 <|> parseSteadyState

-- parseLenormand2012 :: (P.Stream s m Char) => P.ParsecT s u m (V.Vector (Weight, V.Vector Double) -> Run)
-- parseLenormand2012 = do
--   parserSkipDirname
--   _ <- P.string "lenormand2012"
--   n <- P.char '_' *> parserInt
--   alpha <- P.char '_' *> parserDouble
--   pAccMin <- P.char '_' *> parserDouble
--   step <- P.char '_' *> parserInt
--   replication <- P.char '_' *> parserInt
--   _ <- P.string ".csv"
--   return $ Run (Lenormand2012 n alpha pAccMin) step replication
-- 
-- parseBeaumont2009 :: (P.Stream s m Char) => P.ParsecT s u m (V.Vector (Weight, V.Vector Double) -> Run)
-- parseBeaumont2009 = do
--   _ <- parserSkipDirname *> P.string "beaumont2009"
--   n <- P.char '_' *> parserInt
--   epsilonFrom <- P.char '_' *> parserDouble
--   epsilonTo <- P.char '_' *> parserDouble
--   step <- P.char '_' *> parserInt
--   replication <- P.char '_' *> parserInt
--   _ <- P.string ".csv"
--   return $ Run (Beaumont2009 n epsilonFrom epsilonTo) step replication
-- 
-- parseSteadyState :: (P.Stream s m Char) => P.ParsecT s u m (V.Vector (Weight, V.Vector Double) -> Run)
-- parseSteadyState = do
--   _ <- parserSkipDirname *> P.string "steadyState"
--   n <- P.char '_' *> parserInt
--   alpha <- P.char '_' *> parserDouble
--   pAccMin <- P.char '_' *> parserDouble
--   parallel <- P.char '_' *> parserInt
--   step <- P.char '_' *> parserInt
--   replication <- P.char '_' *> parserInt
--   _ <- P.string ".csv"
--   return $ Run (SteadyState n alpha pAccMin parallel) step replication

parserSkipDirname :: (P.Stream s m Char) => P.ParsecT s u m ()
parserSkipDirname = P.optional $ P.many (P.try $ P.many (P.noneOf "/") <> P.string "/")

show2dec :: Double -> Text
show2dec = sformat (fixed 2)

