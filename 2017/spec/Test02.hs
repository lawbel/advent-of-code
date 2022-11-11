module Test02 where

import Common
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Data.Vector qualified as Vec

import Day02 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [part1Tests]

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests"
    [ HUnit.testCase "row 1" $
        Day02.maxDiff (sheet Vec.! 0) @?= the @Int 8
    , HUnit.testCase "row 2" $
        Day02.maxDiff (sheet Vec.! 1) @?= the @Int 4
    , HUnit.testCase "row 3" $
        Day02.maxDiff (sheet Vec.! 2) @?= the @Int 6
    , HUnit.testCase "checksum" $
        Day02.checksum sheet @?= 18 ]
  where
    sheet = Vec.fromList
        [ Vec.fromList [ 5, 1, 9, 5 ]
        , Vec.fromList [ 7, 5, 3    ]
        , Vec.fromList [ 2, 4, 6, 8 ] ]
