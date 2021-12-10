{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test10 where

import Data.Text (Text)
import Optics (preview, _Left, _Right)
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.InterpolatedString.Perl6 (qq)
import Text.Megaparsec qualified as Par

import Day10 (Kind)
import Day10 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [corruptedTests, incompleteTests]

corruptedTests :: TestTree
corruptedTests = Tasty.testGroup "corrupted lines"
    [ corExampleLineTests
    , corScoreTests
    ]

corExampleLineTests :: TestTree
corExampleLineTests = Tasty.testGroup "parse errors" $ do
    (i, (text, eKind, aKind, _)) <- zip [1..] corrupted
    let actual = do
            line <- Par.parseMaybe Day10.parseLine text
            parseErr <- preview _Left $ Day10.getChunks line
            errType <- Day10.checkParseError parseErr
            preview _Right errType
    let expected = Day10.MkCorrupted
            { Day10.expected = Day10.MkDelim Day10.Close eKind
            , Day10.actual = Day10.MkDelim Day10.Close aKind
            }
    pure $ HUnit.testCase [qq|line {i :: Int}|] $ actual @?= Just expected

corScoreTests :: TestTree
corScoreTests = Tasty.testGroup "scores" $ do
    (i, (text, _, _, expected)) <- zip [1..] corrupted
    let actual = do
            line <- Par.parseMaybe Day10.parseLine text
            parseErr <- preview _Left $ Day10.getChunks line
            errType <- Day10.checkParseError parseErr
            Day10.scoreCorrupted <$> preview _Right errType
    pure $ HUnit.testCase [qq|line {i :: Int}|] $ actual @?= Just expected

incompleteTests :: TestTree
incompleteTests = Tasty.testGroup "incomplete lines"
    [ incExampleLineTests
    , incScoreTests
    ]

incExampleLineTests :: TestTree
incExampleLineTests = Tasty.testGroup "recovery" $ do
    (i, (text, complete, _)) <- zip [1..] incomplete
    let actual = do
            line <- Par.parseMaybe Day10.parseLine text
            pure $ line <> Day10.recoverIncomplete line
    let expected = text <> complete
    pure $ HUnit.testCase [qq|line {i :: Int}|] $
        fmap Day10.prettyLine actual @?= Just expected

incScoreTests :: TestTree
incScoreTests = Tasty.testGroup "scores" $ do
    (i, (text, _, score)) <- zip [1..] incomplete
    let actual = do
            line <- Par.parseMaybe Day10.parseLine text
            pure $ Day10.scoreIncomplete $ Day10.recoverIncomplete line
    pure $ HUnit.testCase [qq|line {i :: Int}|] $ actual @?= Just score

corrupted :: [(Text, Kind, Kind, Int)]
corrupted =
    [ ("{([(<{}[<>[]}>{[]{[(<()>", Day10.Square, Day10.Brace, 1197)
    , ("[[<[([]))<([[{}[[()]]]", Day10.Square, Day10.Paren, 3)
    , ("[{[{({}]{}}([{[{{{}}([]", Day10.Paren, Day10.Square, 57)
    , ("[<(<(<(<{}))><([]([]()", Day10.Angled, Day10.Paren, 3)
    , ("<{([([[(<>()){}]>(<<{{", Day10.Square, Day10.Angled, 25137)
    ]

incomplete :: [(Text, Text, Int)]
incomplete =
    [ ("[({(<(())[]>[[{[]{<()<>>", "}}]])})]", 288957)
    , ("[(()[<>])]({[<{<<[]>>(", ")}>]})", 5566)
    , ("(((({<>}<{<{<>}{[]{[]{}", "}}>}>))))", 1480781)
    , ("{<[[]]>}<{[{[{[]{()[[[]", "]]}}]}]}>", 995444)
    , ("<{([{{}}[<[[[<>{}]]]>[]]", "])}>", 294)
    ]
