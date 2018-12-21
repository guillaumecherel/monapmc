{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Util.Parser where
  
import Protolude hiding ((<|>), option, try)

import Text.Parsec
import Text.Parsec.Token

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

parserInt :: (Stream s m Char) => ParsecT s u m Int
parserInt = fromIntegral <$> integer emptyTokenParser
  -- readInt <$> (P.option "" (P.string "-") <> P.many1 P.digit)
  -- where readInt :: Text ->
        -- readInt t = case signed decimal t of
                      -- Right x -> pure x
                      -- Left err -> fail err
 -- read <$> (P.option "" (P.string "-") <> P.many1 P.digit)

parserDouble :: (Stream s m Char) => ParsecT s u m Double
parserDouble = try strictDouble <|> fmap fromIntegral parserInt
  where strictDouble = option identity (try ((string "-") *> pure (* (-1))))
                        <*> float emptyTokenParser
  -- read <$> (P.option "" (P.string "-") <> P.many1 P.digit <> P.option "" (P.string "." <> P.many P.digit) <> P.option "" (P.string "e" <> P.option "" (P.string "-") <> P.many1 P.digit))


