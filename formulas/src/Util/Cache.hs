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



---- 2e Nouvel essai

data Cache a = Cache { cachePath :: FilePath
                       , cacheWrite :: IO ()
                       , cacheRead :: IO (Either Text a)
                       , cacheNeeds :: [FilePath]
                       }

data Sink = Sink { sinkPath :: FilePath
                   , sinkWrite :: IO ()
                   , sinkNeeds :: [FilePath]
                   }
                 
data Builder a = Builder { builderRead :: IO (Either Text a)
                         , builderNeeds :: [FilePath]
                         }

cPure :: a -> Builder a
cPure x = Builder (pure $ pure x) []

cAp :: Builder (a -> b) -> Cache a -> Builder b
cAp f x = Builder ( do
                      ef <- builderRead f
                      ea <- cacheRead x
                      return $ ef <*> ea)
                  (cachePath x : cacheNeeds x ++ builderNeeds f)

cacheAsTxt :: FilePath -> (a -> Text) -> (Text -> Either Text a) -> Builder a -> Cache a
cacheAsTxt path toText fromText b =
  Cache { cachePath = path
         , cacheWrite = do
             eVal <- builderRead b
             case eVal of
               Left err -> fail $ unpack err
               Right val -> writeFile path (toText val)
         , cacheRead = fmap fromText $ readFile path
         , cacheNeeds = builderNeeds b
         }

sinkAs :: FilePath -> (a -> IO ()) -> Builder a -> Sink
sinkAs path write b =
  Sink { sinkPath = path
       , sinkWrite = do
         eVal <- builderRead b
         case eVal of
           Left err -> fail $ unpack err
           Right val -> write val
       , sinkNeeds = builderNeeds b
       }

-- cPure f `cAp` x `cAp` y & cacheAs "z" show read

buildCache :: Cache a -> Rules ()
buildCache x = do
  want [cachePath x]
  cachePath x %> \out -> do
    need $ cacheNeeds x
    traced "Writing cache" $ cacheWrite x

buildSink :: Sink -> Rules ()
buildSink x = do
  want [sinkPath x]
  sinkPath x %> \out -> do
    need $ sinkNeeds x
    traced "Writing sink" $ sinkWrite x

pretty :: Cache a -> Text
pretty a = "Cache path: " <> pack (cachePath a) <> "\n"
        <> "Needs: " <> show (cacheNeeds a)

-- Continuation-passing style
-- cAp :: (a -> b) -> Cache a -> (Builder b -> r) -> r
-- cAp f x k = k $ Builder (f (cacheVal x))
--                         (cachePath x : cacheNeeds x)
-- 
-- cAp' :: Cache a -> Builder (a -> b) -> (Builder b -> r) -> r
-- cAp' x k =
-- 
-- cacheAs :: FilePath -> (a -> Text) -> Builder a -> Cache a
-- cacheAs path toText fromText b = Cache { cacheVal = builderVal b
--                                , cachePath = path
--                                , cacheWrite = toText
--                                , cacheRead = fromText
--                                , cacheNeeds = builderNeeds b }
-- 
-- -- cAp f x $ cAp' y $ cacheAs "z" show

