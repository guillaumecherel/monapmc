{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Util.Cache where
  
import Protolude

import Control.Monad.Fail
import Data.Functor
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text
import Data.Text.Read
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

data Cache a = Cache { cacheRead :: ExceptT Text IO a
                     , cacheNeeds :: Set FilePath
                     , cacheBuild :: Build }
             | CacheFail Text

liftIO :: IO a -> Cache a
liftIO io = Cache (lift io) mempty mempty

instance Functor Cache where
  fmap _ (CacheFail err) = CacheFail err
  fmap f (Cache r n b) = Cache (fmap f r) n b

instance Applicative Cache where
  pure a = Cache (return a) mempty mempty

  (CacheFail err) <*> _ = CacheFail err
  _ <*> (CacheFail err) = CacheFail err
  f <*> a = Cache ( cacheRead f <*> cacheRead a )
                  ( cacheNeeds f <> cacheNeeds a )
                  (  cacheBuild f <> cacheBuild a )

instance (Monoid a) => Monoid (Cache a) where
  mempty = Cache (return mempty) mempty mempty

  mappend (CacheFail err) _ = CacheFail err
  mappend _ (CacheFail err) = CacheFail err
  mappend (Cache cr1 cn1 cb1) (Cache cr2 cn2 cb2) =
    Cache { cacheRead = liftA2 mappend cr1 cr2
          , cacheNeeds = cn1 <> cn2
          , cacheBuild = cb1 <> cb2 }

instance Num a => Num (Cache a) where
    fromInteger = pure . fromInteger
    {-# INLINE fromInteger #-}

    negate = fmap negate
    {-# INLINE negate #-}

    abs = fmap abs
    {-# INLINE abs #-}

    signum = fmap signum
    {-# INLINE signum #-}

    (+) = liftA2 (+)
    {-# INLINE (+) #-}

    (*) = liftA2 (*)
    {-# INLINE (*) #-}

    (-) = liftA2 (-)
    {-# INLINE (-) #-}

instance Fractional a => Fractional (Cache a) where
    fromRational = pure . fromRational
    {-# INLINE fromRational #-}

    recip = fmap recip
    {-# INLINE recip #-}

    (/) = liftA2 (/)
    {-# INLINE (/) #-}

instance Floating a => Floating (Cache a) where
    pi = pure pi
    {-# INLINE pi #-}

    exp = fmap exp
    {-# INLINE exp #-}

    sqrt = fmap sqrt
    {-# INLINE sqrt #-}

    log = fmap log
    {-# INLINE log #-}

    sin = fmap sin
    {-# INLINE sin #-}

    tan = fmap tan
    {-# INLINE tan #-}

    cos = fmap cos
    {-# INLINE cos #-}

    asin = fmap asin
    {-# INLINE asin #-}

    atan = fmap atan
    {-# INLINE atan #-}

    acos = fmap acos
    {-# INLINE acos #-}

    sinh = fmap sinh
    {-# INLINE sinh #-}

    tanh = fmap tanh
    {-# INLINE tanh #-}

    cosh = fmap cosh
    {-# INLINE cosh #-}

    asinh = fmap asinh
    {-# INLINE asinh #-}

    atanh = fmap atanh
    {-# INLINE atanh #-}

    acosh = fmap acosh
    {-# INLINE acosh #-}

    (**) = liftA2 (**)
    {-# INLINE (**) #-}

    logBase = liftA2 logBase
    {-# INLINE logBase #-}

source :: FilePath -> (Text -> Either Text a) -> Cache a
source path fromText = Cache
  { cacheRead = ExceptT $ fromText <$> readFile path
  , cacheNeeds = Set.singleton path
  , cacheBuild = mempty }

need :: FilePath -> Cache ()
need path = Cache
  { cacheRead = return ()
  , cacheNeeds = Set.singleton path
  , cacheBuild = mempty }

cache :: FilePath -> (a -> Text) -> (Text -> Either Text a) -> Cache a
         -> Cache a
cache path toText fromText (CacheFail err) = CacheFail err
cache path toText fromText a = if isBuilt path (cacheBuild a)
  then CacheFail ("The cache file already exists: " <> pack path)
  else Cache { cacheRead = ExceptT $ fromText <$> readFile path
             , cacheNeeds = Set.singleton path
             , cacheBuild = buildSingle path
                              ( toText <$> cacheRead a
                                >>= lift . writeFile path)
                              ( cacheNeeds a )
                         <> cacheBuild a }

cache' :: (Show a, Read a) => FilePath -> Cache a -> Cache a
cache' path = cache path show (bimap pack identity . readEither . unpack)

sink :: FilePath -> (a -> ExceptT Text IO ()) -> Cache a -> Cache ()
sink path write (CacheFail err) = CacheFail err
sink path write a = if isBuilt path (cacheBuild a)
  then CacheFail ("The cache file already exists: " <> pack path)
  else Cache { cacheRead = return ()
             , cacheNeeds = mempty
             , cacheBuild = buildSingle path
                                        (cacheRead a >>= write)
                                        (cacheNeeds a)
                         <> cacheBuild a}

mute :: Cache a -> Cache ()
mute a = a { cacheRead = return ()
           , cacheNeeds = mempty }

sinkIO :: FilePath -> (a -> IO ()) -> Cache a -> Cache ()
sinkIO path write = sink path (lift . write)

sinkTxt :: FilePath -> (a -> Either Text Text) -> Cache a -> Cache ()
sinkTxt path toText = sink path (\a -> ExceptT (return $ toText a) >>= lift . writeFile path)

buildCache :: Cache a -> Rules ()
buildCache (CacheFail err) = action $ fail $ unpack err
buildCache a = build $ cacheBuild a

prettyCache :: (Show a) => Cache a -> Text
prettyCache (Cache _ n b) = "Cache Reads = ?" <> "\n"
                         <> "Cache Needs: \n"
                         <> foldMap (\p -> "  " <> show p <> "\n") n
                         <> "Cache Build: \n"
                         <> foldMap (\l -> "  " <> l <> "\n")
                                    (lines $ prettyBuild b)
prettyCache (CacheFail err) = "CacheFail " <> err

prettyBuild :: Build -> Text
prettyBuild (Build a) = foldMap showOne $ Map.toList a
  where showOne :: (FilePath, (ExceptT Text IO (), Set FilePath)) -> Text
        showOne (target, (_, needs)) =
             pack target <> "\n"
          <> foldMap (\n -> "  " <> pack n <> "\n") needs

newtype Build = Build (Map FilePath (ExceptT Text IO (), Set FilePath))

instance Monoid Build where
  mempty = Build Map.empty
  mappend (Build a) (Build b) = Build $ mappend a b

isBuilt :: FilePath -> Build -> Bool
isBuilt p (Build m) = Map.member p m

buildSingle :: FilePath -> ExceptT Text IO () -> Set FilePath -> Build
buildSingle path write needs = Build $ Map.singleton path (write, needs)

buildList :: Build -> [(FilePath, ExceptT Text IO (), Set FilePath)]
buildList (Build m) = fmap (\(a, (b, c)) -> (a,b,c)) (Map.toList m)

buildTargets :: Build -> [FilePath]
buildTargets (Build m) = Map.keys m

build :: Build -> Rules ()
build b = do
            want (buildTargets b)
            foldMap buildOne ( buildList b )
  where buildOne (outPath, write, needs) =
                    outPath %> \out -> do
                      Development.Shake.need (Set.toList needs)
                      e <- traced "Writing cache" $ runExceptT write
                      case e of
                        Right () -> return ()
                        Left err -> fail $ unpack err
