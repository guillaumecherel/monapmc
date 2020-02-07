{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Util where

import Protolude

import qualified Control.Foldl as Fold
import           Control.Foldl (Fold(..))
import           Control.Monad.Random.Lazy
import           Data.Functor.Compose
import qualified Util.Figure as Figure
import           Util.Figure (Figure(..))
import qualified Data.Vector as Vector
import           Data.Vector (Vector)
import qualified Text.Parsec as P
import Formatting

type Weight = Double

liftC :: (Applicative h)
  => (f a -> g b) -> Compose h f a -> Compose h g b
liftC f = Compose . liftA f . getCompose

liftCIn  
  :: (f1 (f2 a) -> f3 (f4 b)) -> Compose f1 f2 a -> Compose f3 f4 b
liftCIn f = Compose . f . getCompose

liftC2 :: (Applicative g)
  => (f1 a -> f2 b -> f3 c)
  -> Compose g f1 a
  -> Compose g f2 b
  -> Compose g f3 c
liftC2 f a b = Compose $ liftA2 f (getCompose a) (getCompose b)

generalizeRand :: (Monad m) => Rand g a -> RandT g m a
generalizeRand = liftRandT . fmap return . runRand

show2dec :: Double -> Text
show2dec = sformat (fixed 2)

nestFold :: Applicative f => Fold a b -> Fold (f a) (f b)
nestFold (Fold s i e) =
    Fold (\xs as -> liftA2 s xs as)
         (pure i)
         (\xs -> fmap e xs)
{-# INLINABLE nestFold #-}

traceMap :: (Show a) => (b -> a) -> b -> b
traceMap f x = traceShow (f x) x

-- parserSkipDirname :: (P.Stream s m Char) => P.ParsecT s u m ()
-- parserSkipDirname = P.optional $ P.many (P.try $ P.many (P.noneOf "/") <> P.string "/")
 

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

