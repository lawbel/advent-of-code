{-# LANGUAGE OverloadedStrings #-}

module Test08 where

import Data.HashMap.Strict qualified as HashMap
import Data.Vector qualified as Vec
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.Megaparsec qualified as Parse

import Day08 qualified
import Day08 (Condition(..))


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [part1Tests]

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests"
    [ HUnit.testCase "can parse instructions" $
        Parse.runParser (Day08.parseInstrs <* Parse.eof) "test" text
            @?= Right instrs
    , HUnit.testCase "can process instructions" $
        Day08.evalInstrs instrs HashMap.empty
            @?= HashMap.fromList [("a", 1), ("c", -10)] ]
  where
    text =
        "b inc 5 if a > 1\n\
        \a inc 1 if b < 5\n\
        \c dec -10 if a >= 1\n\
        \c inc -20 if c == 10\n"
    instrs = Vec.fromList
        [ Day08.MkInstr "b" Day08.Inc 5     ("a" :>: 1)
        , Day08.MkInstr "a" Day08.Inc 1     ("b" :<: 5)
        , Day08.MkInstr "c" Day08.Dec (-10) ("a" :>=: 1)
        , Day08.MkInstr "c" Day08.Inc (-20) ("c" :==: 10) ]
