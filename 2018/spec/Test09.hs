{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Test09 where

import Data.Foldable qualified as Fold
import Data.Map qualified as Map
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.InterpolatedString.Perl6 (qc)

import Day09 (Input)
import Day09 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests"
    [ Tasty.testGroup "example games"
        [ Tasty.testGroup "main example"
            [ Tasty.testGroup "turns" $
                let trace = Day09.debugGame exampleInput
                in  do  (game, output, i) <- zip3 trace exampleOutput [1..]
                        let gameInfo =
                                ( Day09.curPlayer game + 1
                                , Fold.toList $ Day09.marblesCircle game
                                )
                        pure $ HUnit.testCase [qc|turn {i :: Int}|] $
                            gameInfo @?= output
            , HUnit.testCase "scores" $
                let scores = Map.singleton 5 [9, 23]
                    result = Day09.playerScores (Day09.runGame exampleInput)
                in  Map.mapKeys (+ 1) result @?= scores
            ]
        , Tasty.testGroup "other examples" $
            let makeTest desc (players, marbles) score =
                    HUnit.testCase desc $
                        let result = Day09.runGame $ Day09.MkInput
                                { Day09.players = players
                                , Day09.lastMarble = marbles
                                }
                        in  Day09.highScore @Int @Int result @?= score
            in  [ makeTest "example 1" (10, 1618) 8317
                , makeTest "example 2" (13, 7999) 146373
                , makeTest "example 3" (17, 1104) 2764
                , makeTest "example 4" (21, 6111) 54718
                , makeTest "example 5" (30, 5807) 37305
                ]
        ]
    ]

exampleInput :: Input Int Int
exampleInput = Day09.MkInput
    { Day09.players = 9
    , Day09.lastMarble = 25
    }

exampleOutput :: [(Int, [Int])]
exampleOutput =
    [ (1, [0, 1])
    , (2, [0, 2, 1])
    , (3, [0, 2, 1, 3])
    , (4, [0, 4, 2, 1, 3])
    , (5, [0, 4, 2, 5, 1, 3])
    , (6, [0, 4, 2, 5, 1, 6, 3])
    , (7, [0, 4, 2, 5, 1, 6, 3, 7])
    , (8, [0, 8, 4, 2, 5, 1, 6, 3, 7])
    , (9, [0, 8, 4, 9, 2, 5, 1, 6, 3, 7])
    , (1, [0, 8, 4, 9, 2, 10, 5, 1, 6, 3, 7])
    , (2, [0, 8, 4, 9, 2, 10, 5, 11, 1, 6, 3, 7])
    , (3, [0, 8, 4, 9, 2, 10, 5, 11, 1, 12, 6, 3, 7])
    , (4, [0, 8, 4, 9, 2, 10, 5, 11, 1, 12, 6, 13, 3, 7])
    , (5, [0, 8, 4, 9, 2, 10, 5, 11, 1, 12, 6, 13, 3, 14, 7])
    , (6, [0, 8, 4, 9, 2, 10, 5, 11, 1, 12, 6, 13, 3, 14, 7, 15])
    , (7, [0, 16, 8, 4, 9, 2, 10, 5, 11, 1, 12, 6, 13, 3, 14, 7, 15])
    , (8, [0, 16, 8, 17, 4, 9, 2, 10, 5, 11, 1, 12, 6, 13, 3, 14, 7, 15])
    , (9, [0, 16, 8, 17, 4, 18, 9, 2, 10, 5, 11, 1, 12, 6, 13, 3, 14, 7, 15])
    , (1, [0, 16, 8, 17, 4, 18, 9, 19, 2, 10, 5, 11, 1, 12, 6, 13, 3, 14, 7, 15])
    , (2, [0, 16, 8, 17, 4, 18, 9, 19, 2, 20, 10, 5, 11, 1, 12, 6, 13, 3, 14, 7, 15])
    , (3, [0, 16, 8, 17, 4, 18, 9, 19, 2, 20, 10, 21, 5, 11, 1, 12, 6, 13, 3, 14, 7, 15])
    , (4, [0, 16, 8, 17, 4, 18, 9, 19, 2, 20, 10, 21, 5, 22, 11, 1, 12, 6, 13, 3, 14, 7, 15])
    , (5, [0, 16, 8, 17, 4, 18, 19, 2, 20, 10, 21, 5, 22, 11, 1, 12, 6, 13, 3, 14, 7, 15])
    , (6, [0, 16, 8, 17, 4, 18, 19, 2, 24, 20, 10, 21, 5, 22, 11, 1, 12, 6, 13, 3, 14, 7, 15])
    , (7, [0, 16, 8, 17, 4, 18, 19, 2, 24, 20, 25, 10, 21, 5, 22, 11, 1, 12, 6, 13, 3, 14, 7, 15])
    ]
