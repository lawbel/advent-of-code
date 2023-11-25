{-# LANGUAGE OverloadedStrings #-}

module Test02 where

import Data.Map qualified as Map
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as Tasty.HUnit
import Text.Megaparsec qualified as Par

import Day02 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [parseTests, logicTests]

parseTests :: TestTree
parseTests = Tasty.testGroup "parse tests"
    [ Tasty.HUnit.testCase "main example" $
        let input = "abcdef\nbababc\nabbcde\nabcccd\naabcdd\nabcdee\nababab\n"
            output =
                [ "abcdef"
                , "bababc"
                , "abbcde"
                , "abcccd"
                , "aabcdd"
                , "abcdee"
                , "ababab"
                ]
        in  Par.parseMaybe Day02.parseIDs input @?= Just output
    ]

logicTests :: TestTree
logicTests = Tasty.testGroup "logic tests"
    [ counterTests
    , checksumTests
    , pairwiseTests
    ]

counterTests :: TestTree
counterTests = Tasty.testGroup "counter tests"
    [ Tasty.testGroup "example 1" $
        let counter = Day02.count "abcdef"
        in  [ Tasty.HUnit.testCase "one" $ 2 `elemIn` counter @?= False
            , Tasty.HUnit.testCase "two" $ 3 `elemIn` counter @?= False
            ]
    , Tasty.testGroup "example 2" $
        let counter = Day02.count "bababc"
        in  [ Tasty.HUnit.testCase "one" $ counter Map.!? 'a' @?= Just 2
            , Tasty.HUnit.testCase "two" $ counter Map.!? 'b' @?= Just 3
            ]
    , Tasty.testGroup "example 3" $
        let counter = Day02.count "abbcde"
        in  [ Tasty.HUnit.testCase "one" $ counter Map.!? 'b' @?= Just 2
            , Tasty.HUnit.testCase "two" $ 3 `elemIn` counter @?= False
            ]
    , Tasty.testGroup "example 4" $
        let counter = Day02.count "abcccd"
        in  [ Tasty.HUnit.testCase "one" $ counter Map.!? 'c' @?= Just 3
            , Tasty.HUnit.testCase "two" $ 2 `elemIn` counter @?= False
            ]
    , Tasty.testGroup "example 5" $
        let counter = Day02.count "aabcdd"
        in  [ Tasty.HUnit.testCase "one" $ counter Map.!? 'a' @?= Just 2
            , Tasty.HUnit.testCase "two" $ counter Map.!? 'd' @?= Just 2
            , Tasty.HUnit.testCase "three" $ 3 `elemIn` counter @?= False
            ]
    , Tasty.testGroup "example 6" $
        let counter = Day02.count "abcdee"
        in  [ Tasty.HUnit.testCase "one" $ counter Map.!? 'e' @?= Just 2
            ]
    , Tasty.testGroup "example 7" $
        let counter = Day02.count "ababab"
        in  [ Tasty.HUnit.testCase "one" $ counter Map.!? 'a' @?= Just 3
            , Tasty.HUnit.testCase "two" $ counter Map.!? 'b' @?= Just 3
            , Tasty.HUnit.testCase "three" $ 2 `elemIn` counter @?= False
            ]
    ]
  where
    elemIn x dict = x `elem` Map.elems dict

checksumTests :: TestTree
checksumTests = Tasty.testGroup "checksum tests"
    [ Tasty.HUnit.testCase "main example" $
        let input =
                [ "abcdef"
                , "bababc"
                , "abbcde"
                , "abcccd"
                , "aabcdd"
                , "abcdee"
                , "ababab"
                ]
        in  Day02.checksum input @?= 12
    ]

pairwiseTests :: TestTree
pairwiseTests = Tasty.testGroup "pairwise tests"
    [ Tasty.HUnit.testCase "example 1" $
        Day02.numPairwiseNeq "abcde" "axcye" @?= 2
    , Tasty.HUnit.testCase "example 2" $
        Day02.numPairwiseNeq "fghij" "fguij" @?= 1
    , Tasty.HUnit.testCase "example 3" $
        Day02.filterPairwiseEq "fghij" "fguij" @?= "fgij"
    ]
