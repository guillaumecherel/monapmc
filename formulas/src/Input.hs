{-# LANGUAGE FlexibleContexts #-}

module Input where

import Data.Monoid
import Data.Foldable
import qualified Data.Vector as V
import Control.Applicative
import Control.Monad.Random
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Parsec as P
import System.Directory

import Formulas
import Distribution

readDensity :: String -> Either P.ParseError (Double -> Double)
-- readDensity "toyPosterior" = Right $ toyPosterior 0
readDensity x = P.parse parserDensity "" x

readSamplingDouble :: (MonadRandom m) => String -> Either P.ParseError (m Double)
readSamplingDouble x = P.parse parserSamplingFunction "" x

data Model = Toy deriving (Eq, Show)

readModel :: (MonadRandom m) => String -> Either P.ParseError (V.Vector Double -> m (V.Vector Double))
readModel x = do
  m <- P.parse parserModel "" x
  case m of
    Toy -> return $ toyModel

read1DSample :: FilePath -> T.Text -> Either P.ParseError (V.Vector (V.Vector Double))
read1DSample = --mapM ((fmap fst) . TR.double) (T.lines text)
  P.parse parser1DSample

readSimulationResult :: FilePath -> T.Text -> Either P.ParseError SimulationResult
readSimulationResult filename column =
  P.parse parseSimulationFileName ("filename " ++ filename) filename
  <*> read1DSample filename column
  where parseSimulationFileName = P.try parseLenormand2012 <|> P.try parseBeaumont2009 <|> parseSteadyState

readHistogram :: FilePath -> T.Text -> Either P.ParseError [(Double, Double)]
readHistogram = P.parse parserHistogram

loadSimulation :: FilePath -> IO (Either P.ParseError SimulationResult)
loadSimulation f = (TIO.readFile f) >>= return . readSimulationResult f

loadAllSimulations :: FilePath -> IO [SimulationResult]
loadAllSimulations dir = do
  fs <- listDirectory dir
  ess <- traverse loadSimulation $ map (dir ++ ) fs
  return $ foldMap toList ess

parserInt :: (P.Stream s m Char) => P.ParsecT s u m Int
parserInt = read <$> (P.option "" (P.string "-") <> P.many1 P.digit)

parserDouble :: (P.Stream s m Char) => P.ParsecT s u m Double
parserDouble = read <$> (P.option "" (P.string "-") <> P.many1 P.digit <> P.option "" (P.string "." <> P.many P.digit) <> P.option "" (P.string "e" <> P.option "" (P.string "-") <> P.many1 P.digit))

parser1DSample :: (P.Stream s m Char) => P.ParsecT s u m (V.Vector (V.Vector Double))
parser1DSample = fmap (V.fromList . fmap V.singleton) (P.many $ P.try parserDouble <* P.optional P.endOfLine)
  --P.sepBy parserDouble P.endOfLine << P.endOfLine

parserHistogram :: (P.Stream s m Char) => P.ParsecT s u m [(Double, Double)]
parserHistogram = P.many $ P.try line
  where line = (,) <$> parserDouble
                   <*  P.many P.space
                   <*> parserDouble
                   <* P.optional P.endOfLine

parseLenormand2012 :: (P.Stream s m Char) => P.ParsecT s u m (V.Vector (V.Vector Double) -> SimulationResult)
parseLenormand2012 = do
  parserSkipDirname
  P.string "lenormand2012"
  n <- P.char '_' *> parserInt
  alpha <- P.char '_' *> parserDouble
  pAccMin <- P.char '_' *> parserDouble
  step <- P.char '_' *> parserInt
  replication <- P.char '_' *> parserInt
  ext <- P.string ".csv"
  return $ SimulationResult (Lenormand2012 n alpha pAccMin) step replication

parseBeaumont2009 :: (P.Stream s m Char) => P.ParsecT s u m (V.Vector (V.Vector Double) -> SimulationResult)
parseBeaumont2009 = do
  parserSkipDirname *> P.string "beaumont2009"
  n <- P.char '_' *> parserInt
  epsilonFrom <- P.char '_' *> parserDouble
  epsilonTo <- P.char '_' *> parserDouble
  step <- P.char '_' *> parserInt
  replication <- P.char '_' *> parserInt
  P.string ".csv"
  return $ SimulationResult (Beaumont2009 n epsilonFrom epsilonTo) step replication

parseSteadyState :: (P.Stream s m Char) => P.ParsecT s u m (V.Vector (V.Vector Double) -> SimulationResult)
parseSteadyState = do
  parserSkipDirname *> P.string "steadyState"
  n <- P.char '_' *> parserInt
  alpha <- P.char '_' *> parserDouble
  pAccMin <- P.char '_' *> parserDouble
  parallel <- P.char '_' *> parserInt
  step <- P.char '_' *> parserInt
  replication <- P.char '_' *> parserInt
  P.string ".csv"
  return $ SimulationResult (SteadyState n alpha pAccMin parallel) step replication

parserSkipDirname :: (P.Stream s m Char) => P.ParsecT s u m ()
parserSkipDirname = P.optional $ P.many (P.try $ P.many (P.noneOf "/") <> P.string "/")

parserDensity :: (P.Stream s m Char) => P.ParsecT s u m (Double -> Double)
parserDensity =
      P.try (parserUniform >>= \(Uniform u l) -> return (uniformDensity (u, l)))
  <|> (parserNormal >>= \(Normal m v) -> return (normalDensity m v))

parserSamplingFunction :: (P.Stream s m Char, MonadRandom n) => P.ParsecT s u m (n Double)
parserSamplingFunction =
      P.try (parserUniform >>= \(Uniform u l) -> return (uniformRandomSample (u, l)))
   <|> (parserNormal >>= \(Normal m v) -> return (normalRandomSample m v))

parserUniform :: (P.Stream s m Char) => P.ParsecT s u m Uniform
parserUniform = do
  P.string "uniform" *> P.spaces
  lowerBound <- parserDouble <* P.spaces
  upperBound <- parserDouble <* P.spaces
  return $ Uniform lowerBound upperBound

parserNormal :: (P.Stream s m Char) => P.ParsecT s u m Normal
parserNormal = do
  P.string "normal" *> P.spaces
  mean <- parserDouble <* P.spaces
  var <- parserDouble <* P.spaces
  return $ Normal mean var

parserModel :: (P.Stream s m Char) => P.ParsecT s u m Model
parserModel = P.string "toyModel" *> pure Toy

