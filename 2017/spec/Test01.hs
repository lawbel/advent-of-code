module Test01 where

import Common
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
    [ HUnit.testCase "example 1" $
        Day01.captcha [1,1,2,2] @?= the @Int 3
    , HUnit.testCase "example 2" $
        Day01.captcha [1,1,1,1] @?= the @Int 4
    , HUnit.testCase "example 3" $
        Day01.captcha [1,2,3,4] @?= the @Int 0
    , HUnit.testCase "example 4" $
        Day01.captcha [9,1,2,1,2,1,2,9] @?= the @Int 9 ]
