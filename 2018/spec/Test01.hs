{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test01 where

import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as Tasty.HUnit
import Text.Megaparsec qualified as Par

import Day01 qualified
import Flow ((.>))


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "Unit tests" [parseTests, logicTests]

parseTests :: TestTree
parseTests = Tasty.testGroup "Parse tests"
    [ Tasty.HUnit.testCase "main example" $
        Par.parseMaybe Day01.parseFreqChangesCommas "+1, -2, +3, +1"
            @?= Just [1, -2, 3, 1]
    , Tasty.HUnit.testCase "extra example 1" $
        Par.parseMaybe Day01.parseFreqChangesCommas "+1, +1, +1"
            @?= Just [1, 1, 1]
    , Tasty.HUnit.testCase "extra example 2" $
        Par.parseMaybe Day01.parseFreqChangesCommas "+1, +1, -2"
            @?= Just [1, 1, -2]
    , Tasty.HUnit.testCase "extra example 3" $
        Par.parseMaybe Day01.parseFreqChangesCommas "-1, -2, -3"
            @?= Just [-1, -2, -3]
    , Tasty.HUnit.testCase "newline example" $
        Par.parseMaybe Day01.parseFreqChangesLines "+3\n+13\n+4\n-2"
            @?= Just [3, 13, 4, -2]
    ]

logicTests :: TestTree
logicTests = Tasty.testGroup "Logic tests"
    [ Tasty.HUnit.testCase "partial sums" $
        Day01.partialSums @Int [1, -2, 3, 1] @?= [0, 1, -1, 2, 3]
    , Tasty.HUnit.testCase "loop 1" $
        Day01.firstRepeat (makeLoop [1, -2, 3, 1]) @?= Just @Int 2
    ]
  where
    makeLoop = Day01.partialSums .> cycle
