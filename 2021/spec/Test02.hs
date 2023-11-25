module Test02 where

import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Day02 (Command)
import Day02 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests"
    [ Tasty.testGroup "commands"
        [ HUnit.testCase "runs1" $
            Day02.runs1 commands (Day02.MkPosition 0 0 0)
                @?= Day02.MkPosition 15 10 0
        , HUnit.testCase "runs2" $
            Day02.runs2 commands (Day02.MkPosition 0 0 0)
                @?= Day02.MkPosition 15 60 10
        ]
    ]

commands :: [Command Int]
commands =
    [ Day02.Forward 5
    , Day02.Down 5
    , Day02.Forward 8
    , Day02.Up 3
    , Day02.Down 8
    , Day02.Forward 2
    ]
