module Test07 where

import Data.List.NonEmpty (NonEmpty((:|)))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Day07 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests"
    [ HUnit.testCase "centre" $
        Day07.centre exampleCrabs @?= (2, 37)
    ]

exampleCrabs :: NonEmpty Int
exampleCrabs = 16 :| [1, 2, 0, 4, 2, 7, 1, 2, 14]
