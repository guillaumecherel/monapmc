{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}


module Run where

import Protolude
import qualified Control.Foldl as L
import qualified Data.Map as Map
import Data.Text (intercalate)
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import Formatting
import qualified Text.Parsec as P
import Util.Cache
import Util.Parser
import System.Directory

import Algorithm
import Statistics

data Run = Run
  { getAlgorithm :: Algorithm
  , getStep :: Int
  , getReplication:: Int
  , getSample:: V.Vector (V.Vector Double)
  }
  deriving (Show, Read, Eq, Ord)


--------
-- Descriptive statistics and quantities over a single simulation result
--------
 
nSimus :: Run -> Int
nSimus Run {getAlgorithm=Lenormand2012 {getN=n, getAlpha=alpha}, getStep=step} = numberSimusLenormand2012 n (floor $ fromIntegral n * alpha) step
nSimus Run {getStep=step} = numberSimusSteadyState step

numberSimusLenormand2012 :: Int -> Int -> Int -> Int
numberSimusLenormand2012 n nAlpha step = n + (n - nAlpha) * step

numberSimusSteadyState :: Int -> Int
numberSimusSteadyState step = step




--------
-- Descriptive statistics and quantities over sets of simulations (Folds)
-------

posteriorL2Mean :: Double -> Double -> Int -> (Double -> Double) -> L.Fold Run Double
posteriorL2Mean lowerBound upperBound bins density = L.premap (posteriorL2 lowerBound upperBound bins density . V.toList . fmap V.head getSample) L.mean

nSimusMean :: L.Fold Run Double
nSimusMean = L.premap (fromIntegral . nSimus) L.mean

alphaMean :: L.Fold Run Double
alphaMean = L.premap (getAlpha . getAlgorithm) L.mean

alphaFirst :: L.Fold Run (Maybe Double)
alphaFirst = (fmap . fmap) (getAlpha . getAlgorithm) L.head

pAccMinMean :: L.Fold Run Double
pAccMinMean = L.premap (getPAccMin . getAlgorithm) L.mean

pAccMinFirst :: L.Fold Run (Maybe Double)
pAccMinFirst = (fmap . fmap) (getPAccMin . getAlgorithm) L.head

-- l2VsNSimus :: Double -> Double -> Int -> (Double -> Double) -> L.Fold Run [(Double, Double)]
-- l2VsNSimus lowerBound upperBound bins density = Map.elems <$> groupReplications ((,) <$> numberSimusMean <*> posteriorL2Mean lowerBound upperBound bins density )
--   
-- l2VsAlpha :: Double -> Double -> Int -> (Double -> Double) -> L.Fold Run [(Double, Double)]
-- l2VsAlpha lowerBound upperBound bins density = Map.elems <$> groupReplications ((,) <$> alphaMean <*> posteriorL2Mean lowerBound upperBound bins density )
-- 
-- nSimusVsAlpha :: L.Fold Run [(Double, Double)]
-- nSimusVsAlpha = (Map.elems <$> groupReplications ((,) <$> numberSimusMean <*> alphaMean))
-- 



--------
-- Filter simulation results 
--------

filterLastStep :: [Run] -> [Run]
filterLastStep = L.fold $ L.Fold step Map.empty Map.elems
  where step acc x = let k = forgetStep x
                     in Map.alter (alterVal x) k acc
        forgetStep s = (getAlgorithm s, getReplication s)
        alterVal x1 Nothing = Just x1
        alterVal x1 (Just x2) = if getStep x1 < getStep x2
                                   then Just x2
                                   else Just x1

filterLenormand2012 :: [Run] -> [Run]
filterLenormand2012 = L.fold $ L.Fold step [] identity
  where step acc x = case getAlgorithm x of
                       Lenormand2012{} -> x:acc
                       _ -> acc

filterSteadyState :: [Run] -> [Run]
filterSteadyState = L.fold $ L.Fold step [] identity
  where step acc x = case getAlgorithm x of
                       SteadyState{} -> x:acc
                       _ -> acc

 

--------
-- Group
--------

groupReplications :: L.Fold Run r -> L.Fold Run (Map.Map Algorithm r)
groupReplications = L.groupBy getAlgorithm



--------
-- Cache
--------

pprint :: Run -> Text
pprint s = pprintAlgorithm (getAlgorithm s)
        <> " step=" <> show (getStep s)
        <> " replication=" <> show (getReplication s)
        <> " sample=" <> show (take 3 $ V.toList $ V.head <$> getSample s)
        <> if length (getSample s) > 3 then "..." else ""

-- runFileName :: Run -> FilePath
-- runFileName s = unpack $
--      algorithmPart (getAlgorithm s) <> "_"
--   <> show (getStep s) <> "_"
--   <> show (getReplication s) <> ".csv"
--   where algorithmPart Lenormand2012{getN=n, getAlpha=alpha, getPAccMin=pAccMin} =
--           "lenormand2012_" <> show n <> "_" <> show2dec alpha <> "_" <> show2dec pAccMin
--         algorithmPart Beaumont2009{getN=n, getEpsilonFrom=eFrom, getEpsilonTo=eTo} =
--           "beaumont2009_" <> show n <> "_" <> show2dec eFrom <> "_" <> show2dec eTo
--         algorithmPart SteadyState{getN=n, getAlpha=alpha, getPAccMin=pAccMin, getParallel=par} =
--           "steadyState_" <> show n <> "_" <> show2dec alpha <> "_" <> show2dec pAccMin <> "_" <> show par

show2dec :: Double -> Text
show2dec = sformat (fixed 2)

cacheRun :: FilePath -> Cache Run -> Cache Run
cacheRun = cache'

cacheSample :: FilePath -> Cache Run -> Cache (V.Vector (V.Vector Double))
cacheSample path =
    cache path w r . fmap getSample
  where w :: V.Vector (V.Vector Double) -> Text
        w = Data.Text.intercalate "\n"
          . fmap (Data.Text.intercalate " " . fmap show . V.toList)
          . V.toList
        r :: Text -> Either Text (V.Vector (V.Vector Double))
        r = bimap show identity . readSample path
--------
-- Parsing
--------

read1DSample :: FilePath -> Text -> Either P.ParseError (V.Vector (V.Vector Double))
read1DSample = --mapM ((fmap fst) . TR.double) (T.lines text)
  P.parse parser1DSample

parser1DSample :: (P.Stream s m Char) => P.ParsecT s u m (V.Vector (V.Vector Double))
parser1DSample = (V.fromList . fmap V.singleton)
             <$> (P.many $ P.try parserDouble <* P.optional P.endOfLine)
             <*  P.eof

readSample :: FilePath -> Text -> Either P.ParseError (V.Vector (V.Vector Double))
readSample = --mapM ((fmap fst) . TR.double) (T.lines text)
  P.parse parser1DSample

parserSample :: (P.Stream s m Char) => P.ParsecT s u m (V.Vector (V.Vector Double))
parserSample = (V.fromList . fmap V.fromList)
           <$> (P.many $ P.try (P.sepBy1 parserDouble (P.char ' '))
                      <* P.optional P.endOfLine)
           <*  P.eof

loadSimulation :: FilePath -> IO (Either P.ParseError Run)
loadSimulation f = (TIO.readFile f) >>= return . readRun f

loadAllSimulations :: FilePath -> IO [Run]
loadAllSimulations dir = do
  fs <- listDirectory dir
  ess <- traverse loadSimulation $ map (dir ++ ) fs
  return $ foldMap toList ess

readRun :: FilePath -> Text -> Either P.ParseError Run
readRun filename column =
  P.parse parseSimulationFileName ("filename " ++ filename) filename
  <*> read1DSample filename column
  where parseSimulationFileName = P.try parseLenormand2012 <|> P.try parseBeaumont2009 <|> parseSteadyState

parseLenormand2012 :: (P.Stream s m Char) => P.ParsecT s u m (V.Vector (V.Vector Double) -> Run)
parseLenormand2012 = do
  parserSkipDirname
  _ <- P.string "lenormand2012"
  n <- P.char '_' *> parserInt
  alpha <- P.char '_' *> parserDouble
  pAccMin <- P.char '_' *> parserDouble
  step <- P.char '_' *> parserInt
  replication <- P.char '_' *> parserInt
  ext <- P.string ".csv"
  return $ Run (Lenormand2012 n alpha pAccMin) step replication

parseBeaumont2009 :: (P.Stream s m Char) => P.ParsecT s u m (V.Vector (V.Vector Double) -> Run)
parseBeaumont2009 = do
  parserSkipDirname *> P.string "beaumont2009"
  n <- P.char '_' *> parserInt
  epsilonFrom <- P.char '_' *> parserDouble
  epsilonTo <- P.char '_' *> parserDouble
  step <- P.char '_' *> parserInt
  replication <- P.char '_' *> parserInt
  P.string ".csv"
  return $ Run (Beaumont2009 n epsilonFrom epsilonTo) step replication

parseSteadyState :: (P.Stream s m Char) => P.ParsecT s u m (V.Vector (V.Vector Double) -> Run)
parseSteadyState = do
  parserSkipDirname *> P.string "steadyState"
  n <- P.char '_' *> parserInt
  alpha <- P.char '_' *> parserDouble
  pAccMin <- P.char '_' *> parserDouble
  parallel <- P.char '_' *> parserInt
  step <- P.char '_' *> parserInt
  replication <- P.char '_' *> parserInt
  P.string ".csv"
  return $ Run (SteadyState n alpha pAccMin parallel) step replication

parserSkipDirname :: (P.Stream s m Char) => P.ParsecT s u m ()
parserSkipDirname = P.optional $ P.many (P.try $ P.many (P.noneOf "/") <> P.string "/")

