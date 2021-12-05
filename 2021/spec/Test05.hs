{-# LANGUAGE OverloadedStrings #-}

module Test05 where

import Data.Function ((&))
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text qualified as Text
import Linear (V2(V2))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Day05 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests"
    [ Tasty.testGroup "cartesianLine"
        [ HUnit.testCase "example 1" $
            Day05.cartesianLine @Int (V2 1 1) (V2 1 3)
                @?= Just (V2 1 1, Right 2)
        , HUnit.testCase "example 2" $
            Day05.cartesianLine @Int (V2 9 7) (V2 7 7)
                @?= Just (V2 7 7, Left 2)
        , HUnit.testCase "example 3" $
            Day05.cartesianLine @Int (V2 0 0) (V2 8 8)
                @?= Nothing
        ]
    , Tasty.testGroup "plotCartesianLine"
        [ HUnit.testCase "example 1" $
            Day05.plotCartesianLine @Int (V2 1 1) (Right 2)
                @?= Map.fromList [ (V2 1 y, 1) | y <- [1..3] ]
        , HUnit.testCase "example 2" $
            Day05.plotCartesianLine @Int (V2 7 7) (Left 2)
                @?= Map.fromList [ (V2 x 7, 1) | x <- [7..9] ]
        ]
    , HUnit.testCase "prettyGrid" $
        let actual = exampleLines
                & mapMaybe (uncurry Day05.cartesianLine)
                & Day05.plotCartesianLines
                & Day05.prettyGrid
            expected = Text.intercalate "\n"
                [ ".......1.."
                , "..1....1.."
                , "..1....1.."
                , ".......1.."
                , ".112111211"
                , ".........."
                , ".........."
                , ".........."
                , ".........."
                , "222111...."
                ]
        in  actual @?= expected
    , HUnit.testCase "numPointsWith" $
        Day05.numPointsWith (>= 2) exampleLines @?= 5
    ]

exampleLines :: [(V2 Int, V2 Int)]
exampleLines =
    [ (V2 0 9, V2 5 9)
    , (V2 8 0, V2 0 8)
    , (V2 9 4, V2 3 4)
    , (V2 2 2, V2 2 1)
    , (V2 7 0, V2 7 4)
    , (V2 6 4, V2 2 0)
    , (V2 0 9, V2 2 9)
    , (V2 3 4, V2 1 4)
    , (V2 0 0, V2 8 8)
    , (V2 5 5, V2 8 2)
    ]
