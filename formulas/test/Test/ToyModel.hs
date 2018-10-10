module Test.ToyModel where

import Control.Monad (replicateM)
import qualified Control.Monad.Random as CMR
import Control.Monad.Random (MonadRandom, Rand, getRandomR, evalRand)
import qualified Data.Vector as V
import Statistics.Sample as SS (variance, mean)
import Test.QuickCheck
import Test.QuickCheck.Assertions
import System.Random (mkStdGen)

import Model
import Test.Util

-- Check toy model has mean theta and variance 0.5 * 1 + 0.5 * 0.01
prop_toyModelMeanVar :: Seed -> Double -> Test.QuickCheck.Assertions.Result
prop_toyModelMeanVar (Seed seed) theta = (toyMean, toyVar) ?~== (theta, 0.5 * 1 + 0.5 * 0.01)
  where toyMean :: Double
        toyMean = SS.mean thetas
        toyVar :: Double
        toyVar = SS.variance thetas
        thetas :: V.Vector Double
        thetas = V.fromList $ fmap V.head $ evalRand (replicateM 1000 (toyModel (V.singleton theta))) (mkStdGen seed)

runTests :: IO ()
runTests = do
  -- quickCheck prop_toyModelMeanVar
  return ()

