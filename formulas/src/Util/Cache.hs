{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Util.Cache where
  
import Protolude

import Control.Monad.Fail
import qualified Data.Map as Map
import Data.Text
import Data.Text.Read
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

data Cache a = Cache { cacheRead :: ExceptT Text IO a
                     , cacheNeeds :: [FilePath]
                     , cacheBuild :: Build }

instance Functor Cache where
  fmap f x = x { cacheRead = fmap f (cacheRead x) }

instance Applicative Cache where
  pure a = Cache (return a) [] mempty
  f <*> a = Cache ( cacheRead f <*> cacheRead a )
                  ( cacheNeeds a <> cacheNeeds f )
                  ( cacheBuild a <> cacheBuild f )

source :: FilePath -> (Text -> Either Text a) -> Cache a
source path fromText = Cache
  { cacheRead = ExceptT $ fromText <$> readFile path
  , cacheNeeds = [path]
  , cacheBuild = mempty }

cache :: FilePath -> (a -> Text) -> (Text -> Either Text a) -> Cache a
         -> Cache a
cache path toText fromText a = Cache
  { cacheRead = ExceptT $ fromText <$> readFile path
  , cacheNeeds = path:cacheNeeds a
  , cacheBuild = buildSingle path
                        (toText <$> cacheRead a >>= lift . writeFile path)
                        (cacheNeeds a)
                <> cacheBuild a }

sink :: FilePath -> ExceptT Text IO () -> [FilePath] -> Cache ()
sink path write needs = Cache
  { cacheRead = return ()
  , cacheNeeds = []
  , cacheBuild = buildSingle path write needs }
 
buildCache :: Cache a -> Rules ()
buildCache a = build $ cacheBuild a

prettyCache :: Cache a -> Text
prettyCache a = "Cache { cacheRead=?; cacheNeeds=" <> show (cacheNeeds a)
        <> "; chacheBuild" <> prettyBuild (cacheBuild a) <> " }"

prettyBuild :: Build -> Text
prettyBuild (Build a) = show $ Map.map (\(_, b) -> ("?", b)) a

newtype Build = Build (Map FilePath (ExceptT Text IO (), [FilePath]))

instance Monoid Build where
  mempty = Build Map.empty
  mappend (Build a) (Build b) = Build $ mappend a b

buildSingle :: FilePath -> ExceptT Text IO () -> [FilePath] -> Build
buildSingle path write needs = Build $ Map.singleton path (write, needs)

buildList :: Build -> [(FilePath, ExceptT Text IO (), [FilePath])]
buildList (Build m) = fmap (\(a, (b, c)) -> (a,b,c)) $ Map.toList m

build :: Build -> Rules ()
build s = foldMap buildOne (buildList s)
  where buildOne (outPath, write, needs) = do
          want [outPath]
          outPath %> \out -> do
            need needs
            e <- traced "Writing cache" $ runExceptT $ write
            case e of
              Right () -> return ()
              Left err -> fail $ unpack err



