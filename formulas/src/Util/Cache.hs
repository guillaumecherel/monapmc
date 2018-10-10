{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Util.Cache where
  
import Protolude

import Control.Monad.Fail
import Data.Text
import Data.Text.Read
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util


------


data Uncached a = Uncached { uncachedRead :: ExceptT Text IO a
                           , uncachedNeeds :: [FilePath] }

instance Functor Uncached where
  fmap f x = x { uncachedRead = fmap f (uncachedRead x) }

instance Applicative Uncached where
  pure a = Uncached (return a) []
  f <*> a = Uncached ( uncachedRead f <*> uncachedRead a )
                     ( uncachedNeeds a <> uncachedNeeds f )

data Cache a = Cache { cachePath :: FilePath
                       , cacheWrite :: ExceptT Text IO ()
                       , cacheRead :: ExceptT Text IO a
                       , cacheNeeds :: [FilePath] }

data Sink = Sink { sinkPath :: FilePath
                   , sinkWrite :: ExceptT Text IO ()
                   , sinkNeeds :: [FilePath]
                   }
                 
uncached :: Cache a -> Uncached a
uncached c = Uncached { uncachedRead = cacheRead c
                     , uncachedNeeds = cachePath c:cacheNeeds c }

cPure :: a -> Uncached a
cPure x = pure x 

cAp :: Uncached (a -> b) -> Cache a -> Uncached b
cAp f x = f <*> uncached x

cacheAsTxt :: FilePath -> (a -> Text) -> (Text -> Either Text a) -> Uncached a -> Cache a
cacheAsTxt path toText fromText b =
  Cache { cachePath = path
         , cacheWrite = do
             val <- uncachedRead b
             lift $ writeFile path (toText val)
             return ()
         , cacheRead = ExceptT $ fromText <$> readFile path
         , cacheNeeds = uncachedNeeds b
         }

sinkAs :: FilePath -> (a -> IO ()) -> Uncached a -> Sink
sinkAs path write b =
  Sink { sinkPath = path
       , sinkWrite = do
           val <- uncachedRead b
           lift $ write val
           return ()
       , sinkNeeds = uncachedNeeds b
       }

-- cPure f `cAp` x `cAp` y & cacheAs "z" show read

buildCache :: Cache a -> Rules ()
buildCache x = do
  want [cachePath x]
  cachePath x %> \out -> do
    need $ cacheNeeds x
    e <- traced "Writing cache" $ runExceptT $ cacheWrite x
    case e of
      Right () -> return ()
      Left err -> fail $ unpack err

buildSink :: Sink -> Rules ()
buildSink x = do
  want [sinkPath x]
  sinkPath x %> \out -> do
    need $ sinkNeeds x
    e <- traced "Writing sink" $ runExceptT $ sinkWrite x
    case e of
      Right () -> return ()
      Left err -> fail $ unpack err

pretty :: Cache a -> Text
pretty a = "Cache path: " <> pack (cachePath a) <> "\n"
        <> "Needs: " <> show (cacheNeeds a)

