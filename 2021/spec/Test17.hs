module Test17 where

import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
-- import Test.Tasty.HUnit ((@?=))
-- import Test.Tasty.HUnit qualified as HUnit

-- import Day17 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" []
