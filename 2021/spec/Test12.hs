{-# LANGUAGE OverloadedStrings #-}

module Test12 where

import Data.List qualified as List
import Data.Text (Text)
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Day12 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [pathTests, pathGivenTests]

pathTests :: TestTree
pathTests = Tasty.testGroup "makePathsBetween"
    [ HUnit.testCase "example one" $
        let paths =
                [ ["start", "A", "b", "A", "c", "A", "end"]
                , ["start", "A", "b", "A", "end"]
                , ["start", "A", "b", "end"]
                , ["start", "A", "c", "A", "b", "A", "end"]
                , ["start", "A", "c", "A", "b", "end"]
                , ["start", "A", "c", "A", "end"]
                , ["start", "A", "end"]
                , ["start", "b", "A", "c", "A", "end"]
                , ["start", "b", "A", "end"]
                , ["start", "b", "end"]
                ]
        in  List.sort (Day12.makePathsBetween "start" "end" example1)
                @?= List.sort paths
    , HUnit.testCase "example two" $
        let paths =
                [ ["start", "HN", "dc", "HN", "end"]
                , ["start", "HN", "dc", "HN", "kj", "HN", "end"]
                , ["start", "HN", "dc", "end"]
                , ["start", "HN", "dc", "kj", "HN", "end"]
                , ["start", "HN", "end"]
                , ["start", "HN", "kj", "HN", "dc", "HN", "end"]
                , ["start", "HN", "kj", "HN", "dc", "end"]
                , ["start", "HN", "kj", "HN", "end"]
                , ["start", "HN", "kj", "dc", "HN", "end"]
                , ["start", "HN", "kj", "dc", "end"]
                , ["start", "dc", "HN", "end"]
                , ["start", "dc", "HN", "kj", "HN", "end"]
                , ["start", "dc", "end"]
                , ["start", "dc", "kj", "HN", "end"]
                , ["start", "kj", "HN", "dc", "HN", "end"]
                , ["start", "kj", "HN", "dc", "end"]
                , ["start", "kj", "HN", "end"]
                , ["start", "kj", "dc", "HN", "end"]
                , ["start", "kj", "dc", "end"]
                ]
        in  List.sort (Day12.makePathsBetween "start" "end" example2)
                @?= List.sort paths
    , HUnit.testCase "example three" $
        length (Day12.makePathsBetween "start" "end" example3) @?= 226
    ]

pathGivenTests :: TestTree
pathGivenTests = Tasty.testGroup "makePathsBetweenGiven"
    [ HUnit.testCase "example one" $
        let paths =
                [ ["start", "A", "b", "A", "b", "A", "c", "A", "end"]
                , ["start", "A", "b", "A", "b", "A", "end"]
                , ["start", "A", "b", "A", "b", "end"]
                , ["start", "A", "b", "A", "c", "A", "b", "A", "end"]
                , ["start", "A", "b", "A", "c", "A", "b", "end"]
                , ["start", "A", "b", "A", "c", "A", "c", "A", "end"]
                , ["start", "A", "b", "A", "c", "A", "end"]
                , ["start", "A", "b", "A", "end"]
                , ["start", "A", "b", "d", "b", "A", "c", "A", "end"]
                , ["start", "A", "b", "d", "b", "A", "end"]
                , ["start", "A", "b", "d", "b", "end"]
                , ["start", "A", "b", "end"]
                , ["start", "A", "c", "A", "b", "A", "b", "A", "end"]
                , ["start", "A", "c", "A", "b", "A", "b", "end"]
                , ["start", "A", "c", "A", "b", "A", "c", "A", "end"]
                , ["start", "A", "c", "A", "b", "A", "end"]
                , ["start", "A", "c", "A", "b", "d", "b", "A", "end"]
                , ["start", "A", "c", "A", "b", "d", "b", "end"]
                , ["start", "A", "c", "A", "b", "end"]
                , ["start", "A", "c", "A", "c", "A", "b", "A", "end"]
                , ["start", "A", "c", "A", "c", "A", "b", "end"]
                , ["start", "A", "c", "A", "c", "A", "end"]
                , ["start", "A", "c", "A", "end"]
                , ["start", "A", "end"]
                , ["start", "b", "A", "b", "A", "c", "A", "end"]
                , ["start", "b", "A", "b", "A", "end"]
                , ["start", "b", "A", "b", "end"]
                , ["start", "b", "A", "c", "A", "b", "A", "end"]
                , ["start", "b", "A", "c", "A", "b", "end"]
                , ["start", "b", "A", "c", "A", "c", "A", "end"]
                , ["start", "b", "A", "c", "A", "end"]
                , ["start", "b", "A", "end"]
                , ["start", "b", "d", "b", "A", "c", "A", "end"]
                , ["start", "b", "d", "b", "A", "end"]
                , ["start", "b", "d", "b", "end"]
                , ["start", "b", "end"]
                ]
        in  List.sort (Day12.makePathsBetweenGiven 1 "start" "end" example1)
                @?= List.sort paths
    , HUnit.testCase "example two" $
        length (Day12.makePathsBetweenGiven 1 "start" "end" example2) @?= 103
    , HUnit.testCase "example three" $
        length (Day12.makePathsBetweenGiven 1 "start" "end" example3) @?= 3509
    ]

example1 :: [(Text, Text)]
example1 =
    [ ("start", "A")
    , ("start", "b")
    , ("A", "c")
    , ("A", "b")
    , ("b", "d")
    , ("A", "end")
    , ("b", "end")
    ]

example2 :: [(Text, Text)]
example2 =
    [ ("dc", "end")
    , ("HN", "start")
    , ("start", "kj")
    , ("dc", "start")
    , ("dc", "HN")
    , ("LN", "dc")
    , ("HN", "end")
    , ("kj", "sa")
    , ("kj", "HN")
    , ("kj", "dc")
    ]

example3 :: [(Text, Text)]
example3 =
    [ ("fs", "end")
    , ("he", "DX")
    , ("fs", "he")
    , ("start", "DX")
    , ("pj", "DX")
    , ("end", "zg")
    , ("zg", "sl")
    , ("zg", "pj")
    , ("pj", "he")
    , ("RW", "he")
    , ("fs", "DX")
    , ("pj", "RW")
    , ("zg", "RW")
    , ("start", "pj")
    , ("he", "WI")
    , ("zg", "he")
    , ("pj", "fs")
    , ("start", "RW")
    ]
