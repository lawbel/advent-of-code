module Test21 where

import Control.Monad.State (runState)
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Day21 (Players)
import Day21 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [exampleGameTests]

exampleGameTests :: TestTree
exampleGameTests = Tasty.testGroup "example game"
    [ HUnit.testCase "winner" $ winner @?= Day21.One
    , HUnit.testCase "scores" $
        Day21.scores game @?=
            Day21.MkPlayers
                { Day21.one = 1000
                , Day21.two = 745 }
    , HUnit.testCase "positions" $
        Day21.positions game @?=
            Day21.MkPlayers
                { Day21.one = 10
                , Day21.two = 3 } ]
  where
    (winner, game) =
        runState (Day21.playUntil 1000) (Day21.makeGame examplePositions)

examplePositions :: Players Int
examplePositions = Day21.MkPlayers
    { Day21.one = 4
    , Day21.two = 8 }
