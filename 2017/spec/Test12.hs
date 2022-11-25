{-# LANGUAGE OverloadedStrings #-}

module Test12 where

import Data.HashSet qualified as HashSet
import Flow ((.>))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.Megaparsec qualified as Parse

import Day12 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [part1Tests]

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests"
    [ HUnit.testCase "can parse pipes" $
        Parse.runParser (Day12.parsePipes <* Parse.eof) "test" text
            @?= Right pipes
    , HUnit.testCase "pipes connect to 0" $
        Day12.connected (Day12.MkID 0) pipes @?= makeIDs [0, 2, 3, 4, 5, 6]
    , HUnit.testCase "pipes connect to 1" $
        Day12.connected (Day12.MkID 1) pipes @?= makeIDs [1] ]
  where
    text =
        "0 <-> 2\n\
        \1 <-> 1\n\
        \2 <-> 0, 3, 4\n\
        \3 <-> 2, 4\n\
        \4 <-> 2, 3, 6\n\
        \5 <-> 6\n\
        \6 <-> 4, 5"
    pipes = makePipe <$>
        [ (0, [2])
        , (1, [1])
        , (2, [0, 3, 4])
        , (3, [2, 4])
        , (4, [2, 3, 6])
        , (5, [6])
        , (6, [4, 5]) ]
    makePipe (input, outputs) = Day12.MkPipe
        { Day12.inputID = Day12.MkID input
        , Day12.outputIDs = makeIDs outputs }
    makeIDs = fmap Day12.MkID .> HashSet.fromList
