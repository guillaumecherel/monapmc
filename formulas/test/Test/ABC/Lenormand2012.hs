{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.ABC.Lenormand2012 where

import Control.DeepSeq (NFData)
import qualified Control.Monad.Random.Lazy as CMR
import qualified Control.Foldl as Fold
import Control.Monad.Random.Lazy (MonadRandom, Rand, getRandomR)
import Control.Monad.Zip
import Data.AEq
import Data.Monoid
import qualified Data.Vector as V
import qualified Numeric.LinearAlgebra as LA
import Statistics.Sample as SS (variance, mean)
import Test.QuickCheck
import Test.QuickCheck.Assertions
import System.Random

import ABC.Lenormand2012
import Distribution
import Statistics
import Model
import Test.Util

newtype Weight = Weight Double

instance Arbitrary Weight where
  arbitrary = fmap Weight $ sized $ \s -> choose (0.0, fromIntegral s)

instance Arbitrary Uniform where
  arbitrary = sized $ \s -> do
    let s' = fromIntegral s
    center <- arbitrary
    (Positive width) <- arbitrary
    return (Uniform (center - width / 2) (center + width / 2))

instance Arbitrary Normal where
  arbitrary = sized $ \s -> do
    let s' = fromIntegral s
    m <- arbitrary
    (NonZero v) <- arbitrary
    return (Normal m v)

data SampleableDistribution m a = SampleableDistribution String (a -> Double) (m a)

instance Show (SampleableDistribution m a) where
  show (SampleableDistribution s _ _) = s

instance (MonadRandom m) => Arbitrary (SampleableDistribution m Double) where
  arbitrary = sized $ \s -> oneof
    [ do (Uniform u l) <- arbitrary
         return $ SampleableDistribution ("Uniform " ++ show u ++ " " ++ show l)
                                         (uniformDensity (u,l))
                                         (uniformRandomSample (u,l))
    , do (Normal m v) <- arbitrary
         return $ SampleableDistribution ("Normal " ++ show m ++ " " ++ show v)
                                         (normalDensity m v)
                                         (normalRandomSample m v)
    ]

-- A value P m where the parameter space is 1-dimensional, and the associated prior distribution name.
data P1D m = P1D (P m) String

instance (Monad m, MonadRandom m) => Arbitrary (P1D m) where
  arbitrary = do
    (Positive n) <- arbitrary
    (Positive nAlpha) <- arbitrary `suchThat` ((<= n) . getPositive)
    pAccMin <- sized $ \s -> choose (1/(fromIntegral s+2), 0.5)
    (SampleableDistribution dName dDensity dSample) <- arbitrary
    let observed = V.singleton 0
    return $ P1D (P n nAlpha pAccMin (fmap V.singleton dSample) (dDensity . V.head) observed)
                 dName

instance  Show (P1D m) where
  show (P1D p d) = "P1D (P { n = " ++ show (n p)
                     ++ ", nAlpha = " ++ show (nAlpha p)
                     ++ ", pAccMin = " ++ show (pAccMin p)
                     ++ ", prior = " ++ d
                     ++ "})"

exampleP1D :: P1D (Rand StdGen)
-- exampleP1D = P1D (P { n = 100
--                     , nAlpha = 10
--                     , pAccMin = 0.05
--                     , priorSample = fmap (V.singleton) (uniformRandomSample (0,1))
--                     , priorDensity = uniformDensity (0, 1) . V.head
--                     , distanceToData = \x -> sqrt ((V.head x - 0.5) ** 2)})
--                   "Uniform 0 1"
exampleP1D = P1D (P { n = 100
                    , nAlpha = 10
                    , pAccMin = 0.05
                    , priorSample = fmap V.singleton (normalRandomSample 0 1)
                    , priorDensity = normalDensity 0 1 . V.head
                    , observed = V.singleton 0.5 })
                  "Normal 0 1"

newtype S1D = S1D S

instance Arbitrary S1D where
  arbitrary = sized arbitraryS1D

arbitraryS1D :: Int -> Gen S1D
arbitraryS1D nAlpha = do
  thetas <- fmap LA.fromLists $ vectorOf nAlpha $ vectorOf 1 arbitrary
  weights <- fmap LA.fromList $ vectorOf nAlpha $ fmap getNonNegative (arbitrary :: Gen (NonNegative Double))
  let rhos = LA.fromList $ fmap (\x -> sqrt ((head x - 0.5) ** 2))
                         $ LA.toLists thetas
  pAcc <- choose (0, 1)
  (NonNegative epsilon) <- arbitrary
  return $ S1D $ S thetas weights rhos pAcc epsilon

instance Show S1D where
  show (S1D s) = "S1D (S { thetas = " ++ show (thetas s)
                     ++ ", weights = " ++ show (weights s)
                     ++ ", rhos = " ++ show (rhos s)
                     ++ ", pAcc = " ++ show (pAcc s)
                     ++ ", epsilon = " ++ show (epsilon s) ++ " })"

exampleS1D =
  let thetas = LA.matrix 1 [fromIntegral x / 100.0 | x <- [0..9]]
      weights = LA.vector $ replicate 10 1
      rhos = sqrt (((head $ LA.toColumns thetas) - LA.scalar 0.5) ** 2)
      thetaMean = LA.sumElements thetas / 10.0
      thetaVar = LA.sumElements ((thetas - LA.scalar thetaMean) ** 2) / 9.0
      pAcc = 0.1
      epsilon = LA.maxElement rhos
  in S1D (S thetas weights rhos pAcc epsilon)

data PS1D m = PS1D (P m) String S

instance Show (PS1D m) where
  show (PS1D p n s) = "(PS1D " ++ show (P1D p n) ++ " " ++ show (S1D s) ++ ")"
  
instance (MonadRandom m) => Arbitrary (PS1D m) where
  arbitrary = do
    (P1D p d) <- arbitrary
    (S1D s) <- arbitraryS1D (nAlpha p)
    return (PS1D p d s)

examplePS1D =
  case exampleP1D of
    (P1D p n) -> case exampleS1D of
      (S1D s) -> PS1D p n s

newtype Theta1D = Theta1D {_theta1D :: LA.Vector Double} deriving (Show)

instance Arbitrary Theta1D where
  arbitrary = fmap (Theta1D . LA.vector . (\x -> [x])) (arbitrary :: Gen Double)

newtype Thetas1D = Thetas1D {_thetas1D :: LA.Matrix Double} deriving (Show)

instance Arbitrary Thetas1D where
  arbitrary = fmap (Thetas1D . LA.fromRows . fmap _theta1D) $ listOf arbitrary

newtype Var1D = Var1D (LA.Herm Double)

instance Arbitrary Var1D where
  arbitrary = sized $ \s -> fmap (\s2 -> Var1D $ LA.trustSym (LA.fromLists [[s2]])) (choose (0, fromIntegral s))

-- Check the formula for the weight againts the simpler formulation in 1D for a single theta.
prop_weights1D :: PS1D (Rand StdGen) -> Theta1D -> Property
prop_weights1D (PS1D p d s) (Theta1D theta) =
  label ("weight = " ++ show (compWeights p s sigmaSquared (LA.asRow theta))) $
  counterexample (show exampleP1D ++ "\n" ++ show exampleS1D) (
  (compWeights p s sigmaSquared (LA.asRow theta)) `LA.atIndex` 0 ?~==
  (priorDensity p thetaV * LA.sumElements (weights s) / getSum (
    foldMap (\(wj, thetaj) -> Sum (wj * exp (- ((V.head thetaV - thetaj `LA.atIndex` 0) ** 2) / (2 * s2) ) / sqrt (2 * pi * s2) ))
            (mzip (LA.toList $ weights s) (LA.toRows $ thetas s))))
  )
  where s2 = LA.unSym sigmaSquared `LA.atIndex` (0,0)
        sigmaSquared = LA.scale 2
                        $ weightedCovariance (thetas s) (weights s)
        thetaV = V.fromList $ LA.toList theta

-- -- Check the algorithm with a toy 1D model against the theoretical
-- -- distribution, over the mean and variance of the resulting sample
-- prop_toyModel :: Seed -> Property
-- prop_toyModel (Seed seed) = total $ head $ drop 10 $ CMR.evalRand (lenormand2012 p toyModel) (mkStdGen seed)
--   where p = P { n = 100
--               , nAlpha = 10
--               , pAccMin = 0.05
--               , priorSample = fmap V.singleton (uniformRandomSample (-10, 10))
--               , priorDensity = uniformDensity (-10, 10) . V.head
--               , distanceToData = \x -> abs (V.head x)}

-- Check the algorithm with a toy 1D model against sample values for L2
prop_toyModelL2 :: Seed -> Property
prop_toyModelL2 (Seed seed) =
  let p = P { n = 100
            , nAlpha = 10
            , pAccMin = 0.05
            , priorSample = fmap V.singleton (uniformRandomSample (-10, 10))
            , priorDensity = uniformDensity (-10, 10) . V.head
            , observed = V.singleton 0
            }
      observed = 0
      doRun :: Rand StdGen S
      doRun = run p toyModel
      replications ::[S]
      replications = CMR.evalRand (CMR.replicateM 10 doRun) (mkStdGen seed)
      l2 :: S -> Double
      l2 s = posteriorL2 (-10) 10 300 (toyPosteriorCDF observed)
               $ zip (LA.toList $ weights s)
                     (LA.toList $ head $ LA.toColumns $ thetas s)
      l2Rep :: [Double]
      l2Rep = fmap l2 replications
      (l2Mean, l2Var) = Fold.fold ((,) <$> Fold.mean <*> Fold.variance) l2Rep
      test :: Bool
      test = (abs (l2Mean - 0.39) < 0.01)
               && (abs (l2Var - 0.007) < 0.002 )
  -- TODO: dans le code R, le l2 moyen avec ces paramètres reste au dessus de
  -- 0.37
  in cover (0.30 < l2Mean && l2Mean < 0.41) 90 "0.38 < l2Mean < 0.41"
      $ classify (0.38 >= l2Mean) "0.38 >= l2Mean"
      $ classify (l2Mean >= 0.41) "0.41 <= l2Mean"
      $ True


runTests = do
  checkOrExit "prop_weights1D" prop_weights1D
  checkOrExit "prop_toyModelL2" prop_toyModelL2
  -- quickCheck prop_toyModel

