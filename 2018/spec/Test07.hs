{-# LANGUAGE OverloadedStrings #-}

module Test07 where

import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.Megaparsec qualified as Par

import Day07 (Instr)
import Day07 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [parseTests, unitTests]

instr1 :: Instr Char
instr1 = Day07.MkInstr 'C' 'A'

instr2 :: Instr Char
instr2 = Day07.MkInstr 'C' 'F'

instr3 :: Instr Char
instr3 = Day07.MkInstr 'A' 'B'

instr4 :: Instr Char
instr4 = Day07.MkInstr 'A' 'D'

instr5 :: Instr Char
instr5 = Day07.MkInstr 'B' 'E'

instr6 :: Instr Char
instr6 = Day07.MkInstr 'D' 'E'

instr7 :: Instr Char
instr7 = Day07.MkInstr 'F' 'E'

parseTests :: TestTree
parseTests = Tasty.testGroup "parse tests"
    [ HUnit.testCase "example 1" $
        let text = "Step C must be finished before step A can begin."
        in  Par.parseMaybe Day07.parseInstr text @?= Just instr1
    , HUnit.testCase "example 2" $
        let text = "Step C must be finished before step F can begin."
        in  Par.parseMaybe Day07.parseInstr text @?= Just instr2
    , HUnit.testCase "example 3" $
        let text = "Step A must be finished before step B can begin."
        in  Par.parseMaybe Day07.parseInstr text @?= Just instr3
    , HUnit.testCase "example 4" $
        let text = "Step A must be finished before step D can begin."
        in  Par.parseMaybe Day07.parseInstr text @?= Just instr4
    , HUnit.testCase "example 5" $
        let text = "Step B must be finished before step E can begin."
        in  Par.parseMaybe Day07.parseInstr text @?= Just instr5
    , HUnit.testCase "example 6" $
        let text = "Step D must be finished before step E can begin."
        in  Par.parseMaybe Day07.parseInstr text @?= Just instr6
    , HUnit.testCase "example 7" $
        let text = "Step F must be finished before step E can begin."
        in  Par.parseMaybe Day07.parseInstr text @?= Just instr7
    ]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests"
    [ HUnit.testCase "order" $
        let instrs = [instr1, instr2, instr3, instr4, instr5, instr6, instr7]
            graph = Day07.instrsToGraph instrs
        in  Day07.order graph @?= Just "CABDFE"
    ]
