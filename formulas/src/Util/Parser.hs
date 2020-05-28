{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Util.Parser where
  
import Protolude hiding ((<|>), option, try, many, optional)

import Text.Parsec
import Text.Parsec.Token

import Experiment (Simulation(..), Algorithm(..), CompParams(..))
import Model (Model(..))
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Util (Weight, strictlyPositive)

emptyDef :: (Stream s m Char) => GenLanguageDef s u m
emptyDef = LanguageDef
           { commentStart   = ""
           , commentEnd     = ""
           , commentLine    = ""
           , nestedComments = True
           , identStart     = letter <|> char '_'
           , identLetter    = alphaNum <|> oneOf "_'"
           , opStart        = opLetter emptyDef
           , opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
           , reservedOpNames= []
           , reservedNames  = []
           , caseSensitive  = True
           }

emptyTokenParser :: (Stream s m Char) => GenTokenParser s u m
emptyTokenParser = makeTokenParser emptyDef

-- Parse p followed by many c.
lex 
  :: (Stream s m Char) 
  => Char -> ParsecT s u m a -> ParsecT s u m a
lex c p = p <* (many (char c))

int :: (Stream s m Char, Num a) => ParsecT s u m a
int = fromIntegral <$> integer emptyTokenParser

-- positive integer
nat :: (Stream s m Char, Num a) => ParsecT s u m a
nat = fromIntegral <$> natural emptyTokenParser

double :: (Stream s m Char) => ParsecT s u m Double
double = option identity (try ((string "-") *> pure (* (-1))))
     <*> positiveDouble

positiveDouble :: (Stream s m Char) => ParsecT s u m Double
positiveDouble = try (float emptyTokenParser) <|> nat

dirName :: (Stream s m Char) => ParsecT s u m FilePath
dirName =
  -- if we have an empty string, return "./"
     (\dir -> bool dir "./" (null dir))
  -- concat all nested directores together
   .  mconcat
  -- Parse many times a word ended by a /
  <$> many (try $ many (noneOf "/") <> string "/")

simulationSpecString :: (Stream s m Char) => ParsecT s u m Simulation
simulationSpecString =
  Simulation <$> algorithm <*> model <*> stepMax
  where
    algorithm = apmc <|> monApmc
    apmc =
      try
       $  APMC
      <$  lex '_' (string "apmc")
      <*> lex '_' (string "nGen" *> int)
      <*> lex '_' (string "nAlpha" *> fmap strictlyPositive int)
      <*> lex '_' (string "pAccMin" *> positiveDouble)
      <*> lex '_' (string "parallel" *> int)
    monApmc =
      try
       $  MonAPMC
      <$  lex '_' (string "mon-apmc")
      <*> lex '_' (string "nGen" *> int)
      <*> lex '_' (string "nAlpha" *> fmap strictlyPositive int)
      <*> lex '_' (string "pAccMin" *> positiveDouble)
      <*> lex '_' (string "stepSize" *> int)
      <*> lex '_' (string "parallel" *> int)
      <*> lex '_' (string "stopSampleSize" *> int)
    model =
      try
       $  lex '_' ( string "model"
                *> choice [toyTimeVar, toyTimeBias, toy]
              )
    toy =
      try
       $  Toy
      <$  lex '_' (string "Toy")
    toyTimeVar =
      try
       $  ToyTimeVar
      <$  lex '_' (string "ToyTimeVar")
      <*> lex '_' positiveDouble
      <*> lex '_' positiveDouble
    toyTimeBias =
      try
       $  ToyTimeBias
      <$  lex '_' (string "ToyTimeBias")
      <*> lex '_' positiveDouble
      <*> lex '_' double
      <*> lex '_' double
      <*> lex '_' positiveDouble
    stepMax =
      try
       $  lex '_' (string "stepMax" *> int)

simulationFileName
  :: (Stream s m Char)
  => ParsecT s u m Simulation
simulationFileName = dirName *> simulationSpecString

compSpecString :: (Stream s m Char) => ParsecT s u m CompParams
compSpecString =
          CompParams
      <$  lex '_' (string "comp")
      <*> lex '_' (string "nGen" *> int)
      <*> lex '_' (string "nAlpha" *> fmap strictlyPositive int)
      <*> lex '_' (string "pAccMin" *> positiveDouble)
      <*> lex '_' (string "parallel" *> int)
      <*> lex '_' (string "stepMax" *> int)
      <*> lex '_' (string "biasFactor" *> positiveDouble)
      <*> lex '_' (string "meanRunTime" *> positiveDouble)
      <*> lex '_' (string "varRunTime" *> positiveDouble)

compFileName :: (Stream s m Char) => ParsecT s u m CompParams
compFileName = dirName *> compSpecString

read1DSample :: FilePath -> Text -> Either ParseError (Vector (Weight, Vector Double))
read1DSample = --mapM ((fmap fst) . TR.double) (T.lines text)
  parse parser1DSample

parser1DSample :: (Stream s m Char) => ParsecT s u m (Vector (Weight, Vector Double))
parser1DSample = Vector.fromList
             <$> (many $ try weightAndValue <* optional endOfLine)
             <*  eof
  where weightAndValue = (\w x -> (w, Vector.singleton x)) <$> double <*> double


---- TO BE REMOVED
--------
-- Parsing Histograms
--------
--
-- readHistogram :: FilePath -> Text -> Either P.ParseError [(Double, Double)]
-- readHistogram = P.parse parserHistogram
-- 
-- parserHistogram :: (P.Stream s m Char) => P.ParsecT s u m [(Double, Double)]
-- parserHistogram = P.many $ P.try line
--   where line = (,) <$> parserDouble
--                    <*  P.many P.space
--                    <*> parserDouble
--                    <* P.optional P.endOfLine

---- TO BE REMOVED
--------
-- Parsing Util.Distribution
--------

-- class Sampleable d a | d -> a where
--   sample :: (MonadRandom m) => d -> m a
-- 
-- instance Sampleable Uniform Double where
--   sample (Uniform {uniformLowerBound = l, uniformUpperBound = u}) = uniformRandomSample (l, u)
-- 
-- instance Sampleable Normal Double where
--   sample (Normal {normalMean = m, normalVar = v}) = normalRandomSample m v
-- 
-- class Continuous d a | d -> a where
--   density :: d -> a -> Double
-- 
-- instance Continuous Uniform Double where
--   density (Uniform l u) = uniformDensity (u, l)
-- 
-- instance Continuous Normal Double where
--   density (Normal m v) = normalDensity m v
-- 
-- class Discrete d a | d -> a where
--   proba :: d -> a -> Double
-- 
-- readDensity :: String -> Either P.ParseError (Double -> Double)
-- -- readDensity "toyPosterior" = Right $ toyPosterior 0
-- readDensity x = P.parse parserDensity "" x
-- 
-- readSamplingDouble :: (MonadRandom m) => String -> Either P.ParseError (m Double)
-- readSamplingDouble x = P.parse parserSamplingFunction "" x
-- 
-- parserDensity :: (P.Stream s m Char) => P.ParsecT s u m (Double -> Double)
-- parserDensity =
--       P.try (parserUniform >>= \(Uniform u l) -> return (uniformDensity (u, l)))
--   <|> (parserNormal >>= \(Normal m v) -> return (normalDensity m v))
-- 
-- parserSamplingFunction :: (P.Stream s m Char, MonadRandom n) => P.ParsecT s u m (n Double)
-- parserSamplingFunction =
--       P.try (parserUniform >>= \(Uniform u l) -> return (uniformRandomSample (u, l)))
--    <|> (parserNormal >>= \(Normal m v) -> return (normalRandomSample m v))
-- 
-- parserUniform :: (P.Stream s m Char) => P.ParsecT s u m Uniform
-- parserUniform = do
--   P.string "uniform" *> P.spaces
--   lowerBound <- parserDouble <* P.spaces
--   upperBound <- parserDouble <* P.spaces
--   return $ Uniform lowerBound upperBound
-- 
-- parserNormal :: (P.Stream s m Char) => P.ParsecT s u m Normal
-- parserNormal = do
--   P.string "normal" *> P.spaces
--   mean <- parserDouble <* P.spaces
--   var <- parserDouble <* P.spaces
--   return $ Normal mean var
-- 
