{-# LANGUAGE OverloadedStrings #-}

module Test08 where

import Data.List.NonEmpty (NonEmpty((:|)))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.Megaparsec qualified as Par

import Day08 (Tree)
import Day08 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [parseTests, unitTests]

vals :: [Int]
vals = [2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2]

exampleTree :: Tree Int
exampleTree = Day08.Node
    { Day08.header = Day08.MkHeader 2 3
    , Day08.children =
        [ Day08.Node
            { Day08.header = Day08.MkHeader 0 3
            , Day08.children = []
            , Day08.metadata = 10 :| [11, 12]
            }
        , Day08.Node
            { Day08.header = Day08.MkHeader 1 1
            , Day08.children =
                [ Day08.Node
                    { Day08.header = Day08.MkHeader 0 1
                    , Day08.children = []
                    , Day08.metadata = 99 :| []
                    }
                ]
            , Day08.metadata = 2 :| []
            }
        ]
    , Day08.metadata = 1 :| [1, 2]
    }

parseTests :: TestTree
parseTests = Tasty.testGroup "parse tests"
    [ HUnit.testCase "parseVals" $
        Par.parseMaybe Day08.parseVals "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
            @?= Just vals
    , HUnit.testCase "parseTree" $
        Par.parseMaybe Day08.parseTree vals @?= Just exampleTree
    ]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests"
    [ HUnit.testCase "sumOfMetadata" $ Day08.sumOfMetadata exampleTree @?= 138
    ]
