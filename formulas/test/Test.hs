import Test.QuickCheck

import qualified Test.ABC.Lenormand2012
import qualified Test.ABC.SteadyState
import qualified Test.ToyModel
import qualified Test.Util.SteadyState

main :: IO ()
main = do
  Test.ABC.Lenormand2012.runTests
  Test.ABC.SteadyState.runTests
  Test.ToyModel.runTests
  Test.Util.SteadyState.runTests
