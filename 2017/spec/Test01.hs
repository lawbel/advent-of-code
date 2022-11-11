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
unitTests = Tasty.testGroup "unit tests" [part1Tests, part2Tests]

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests"
    [ HUnit.testCase "example 1" $
        Day01.captcha [1,1,2,2] @?= the @Int 3
    , HUnit.testCase "example 2" $
        Day01.captcha [1,1,1,1] @?= the @Int 4
    , HUnit.testCase "example 3" $
        Day01.captcha [1,2,3,4] @?= the @Int 0
    , HUnit.testCase "example 4" $
        Day01.captcha [9,1,2,1,2,1,2,9] @?= the @Int 9 ]

part2Tests :: TestTree
part2Tests = Tasty.testGroup "part 2 tests"
    [ HUnit.testCase "example 1" $
        Day01.captcha2 [1,2,1,2] @?= the @Int 6
    , HUnit.testCase "example 2" $
        Day01.captcha2 [1,2,2,1] @?= the @Int 0
    , HUnit.testCase "example 3" $
        Day01.captcha2 [1,2,3,4,2,5] @?= the @Int 4
    , HUnit.testCase "example 4" $
        Day01.captcha2 [1,2,3,1,2,3] @?= the @Int 12
    , HUnit.testCase "example 5" $
        Day01.captcha2 [1,2,1,3,1,4,1,5] @?= the @Int 4 ]
