{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Util.CSV where

import Protolude
import qualified Data.ByteString.Lazy as BSL
import Data.Csv
import Data.Text (pack, intercalate)
-- import Data.Text.Encoding
import qualified Data.Vector as V

encodeText :: ToRecord a => [Text]-> [a] -> Text
encodeText colNames = mappend (Data.Text.intercalate "," colNames <> "\n")
                    . decodeUtf8 . BSL.toStrict . encode

decodeText :: FromRecord a => Text -> Either Text (V.Vector a)
decodeText = first pack . decode HasHeader . BSL.fromStrict . encodeUtf8

