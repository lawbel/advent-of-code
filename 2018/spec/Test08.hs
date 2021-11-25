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

exampleTree :: Tree
exampleTree = nodeA

nodeA :: Tree
nodeA = Day08.Node
    { Day08.header = Day08.MkHeader 2 3
    , Day08.children = [nodeB, nodeC]
    , Day08.metadata = 1 :| [1, 2]
    }

nodeB :: Tree
nodeB = Day08.Node
    { Day08.header = Day08.MkHeader 0 3
    , Day08.children = []
    , Day08.metadata = 10 :| [11, 12]
    }

nodeC :: Tree
nodeC = Day08.Node
    { Day08.header = Day08.MkHeader 1 1
    , Day08.children = [nodeD]
    , Day08.metadata = 2 :| []
    }

nodeD :: Tree
nodeD = Day08.Node
    { Day08.header = Day08.MkHeader 0 1
    , Day08.children = []
    , Day08.metadata = 99 :| []
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
    , Tasty.testGroup "nodeValue"
        [ HUnit.testCase "node A" $ Day08.nodeValue nodeA @?= 66
        , HUnit.testCase "node B" $ Day08.nodeValue nodeB @?= 33
        , HUnit.testCase "node C" $ Day08.nodeValue nodeC @?= 0
        , HUnit.testCase "node D" $ Day08.nodeValue nodeD @?= 99
        ]
    ]
