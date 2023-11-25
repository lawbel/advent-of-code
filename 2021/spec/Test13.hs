{-# LANGUAGE OverloadedStrings #-}

module Test13 where

import Data.Text qualified as Text
import Linear (V2(V2))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Day13 (Fold)
import Day13 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests"
    [ HUnit.testCase "initial" $
        let expected = Text.intercalate "\n"
                [ "...#..#..#."
                , "....#......"
                , "..........."
                , "#.........."
                , "...#....#.#"
                , "..........."
                , "..........."
                , "..........."
                , "..........."
                , "..........."
                , ".#....#.##."
                , "....#......"
                , "......#...#"
                , "#.........."
                , "#.#........"
                ]
        in  Day13.prettyGrid '#' '.' examplePoints @?= expected
    , HUnit.testCase "after y fold" $
        let expected = Text.intercalate "\n"
                [ "#.##..#..#."
                , "#...#......"
                , "......#...#"
                , "#...#......"
                , ".#.#..#.###"
                ]
            actual = Day13.applyFolds (take 1 exampleFolds) examplePoints
        in  Day13.prettyGrid '#' '.' actual @?= expected
    , HUnit.testCase "after x and y folds" $
        let expected = Text.intercalate "\n"
                [ "#####"
                , "#...#"
                , "#...#"
                , "#...#"
                , "#####"
                ]
            actual = Day13.applyFolds exampleFolds examplePoints
        in  Day13.prettyGrid '#' '.' actual @?= expected
    ]

examplePoints :: [V2 Int]
examplePoints =
    [ V2 6 10
    , V2 0 14
    , V2 9 10
    , V2 0 3
    , V2 10 4
    , V2 4 11
    , V2 6 0
    , V2 6 12
    , V2 4 1
    , V2 0 13
    , V2 10 12
    , V2 3 4
    , V2 3 0
    , V2 8 4
    , V2 1 10
    , V2 2 14
    , V2 8 10
    , V2 9 0
    ]

exampleFolds :: [Fold Int]
exampleFolds =
    [ Day13.MkFold Day13.Y 7
    , Day13.MkFold Day13.X 5
    ]
