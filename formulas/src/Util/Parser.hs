{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Util.Parser where
  
import Protolude
import Prelude (read)

import qualified Text.Parsec as P

parserInt :: (P.Stream s m Char) => P.ParsecT s u m Int
parserInt = read <$> (P.option "" (P.string "-") <> P.many1 P.digit)

parserDouble :: (P.Stream s m Char) => P.ParsecT s u m Double
parserDouble = read <$> (P.option "" (P.string "-") <> P.many1 P.digit <> P.option "" (P.string "." <> P.many P.digit) <> P.option "" (P.string "e" <> P.option "" (P.string "-") <> P.many1 P.digit))


