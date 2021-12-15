{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Test15 where

import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Day15 qualified
import Common.Matrix


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [aStarTests]

aStarTests :: TestTree
aStarTests = Tasty.testGroup "aStar"
    [ HUnit.testCase "example" $
        let from = (xMin exampleGrid, yMin exampleGrid)
            to = (xMax exampleGrid, yMax exampleGrid)
        in  Day15.aStar from to exampleGrid @?= Day15.Finite 40
    ]

exampleGrid :: Matrix 10 10 Int
exampleGrid =
    $$(
        liftMatrix
            [ [1, 1, 6, 3, 7, 5, 1, 7, 4, 2]
            , [1, 3, 8, 1, 3, 7, 3, 6, 7, 2]
            , [2, 1, 3, 6, 5, 1, 1, 3, 2, 8]
            , [3, 6, 9, 4, 9, 3, 1, 5, 6, 9]
            , [7, 4, 6, 3, 4, 1, 7, 1, 1, 1]
            , [1, 3, 1, 9, 1, 2, 8, 1, 3, 7]
            , [1, 3, 5, 9, 9, 1, 2, 4, 2, 1]
            , [3, 1, 2, 5, 4, 2, 1, 6, 3, 9]
            , [1, 2, 9, 3, 1, 3, 8, 5, 2, 1]
            , [2, 3, 1, 1, 9, 4, 4, 5, 8, 1]
            ]
    )
