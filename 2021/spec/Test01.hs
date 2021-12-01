module Test01 where

import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Day01 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests"
    [ HUnit.testCase "increases" $
        Day01.increases depths @?= [200, 208, 210, 207, 240, 269, 263]
    ]

depths :: [Int]
depths = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
