{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Test09 where

import Data.Bifunctor (bimap)
import Data.Function ((&))
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Vector qualified as Vec
import Language.Haskell.TH.Syntax (liftTyped)
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Day09 qualified
import Common.Matrix (Matrix)
import Common.Matrix qualified as Mat


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests"
    [ Tasty.testGroup "example"
        [ HUnit.testCase "lowPoints" $
            let expected = Map.fromList
                    [ ((1, 0), 1)
                    , ((2, 2), 5)
                    , ((6, 4), 5)
                    , ((9, 0), 0)
                    ]
                actual = Day09.lowPoints exampleMatrix
                    & Map.mapKeys (bimap Mat.unFin Mat.unFin)
            in  actual @?= expected
        , HUnit.testCase "sumOfRiskOfLowPoints" $
            Day09.sumOfRiskOfLowPoints exampleMatrix @?= 15
        ]
    ]

exampleMatrix :: Matrix 10 5 Int
exampleMatrix =
    $$(
        liftTyped $ fromJust $ Mat.fromVectors $ Vec.fromList
            [ Vec.fromList [2, 1, 9, 9, 9, 4, 3, 2, 1, 0]
            , Vec.fromList [3, 9, 8, 7, 8, 9, 4, 9, 2, 1]
            , Vec.fromList [9, 8, 5, 6, 7, 8, 9, 8, 9, 2]
            , Vec.fromList [8, 7, 6, 7, 8, 9, 6, 7, 8, 9]
            , Vec.fromList [9, 8, 9, 9, 9, 6, 5, 6, 7, 8]
            ]
    )
