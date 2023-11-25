{-# LANGUAGE OverloadedStrings #-}

module Test09 where

import Data.Bifunctor (second)
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.InterpolatedString.Perl6 (qq, q)
import Text.Megaparsec qualified as Parse

import Day09 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [part1Tests, part2Tests]

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests" $ do
    (i, (text, score)) <- zip @Int [1..] examples
    let parsed = Parse.runParser (Day09.parseGroup <* Parse.eof) "test" text
    pure $ HUnit.testCase [qq|example $i|] $
        second Day09.scoreGroups parsed @?= Right score
  where
    examples =
        [ ("{}", 1)
        , ("{{{}}}", 6)
        , ("{{},{}}", 5)
        , ("{{{},{},{{}}}}", 16)
        , ("{<a>,<a>,<a>,<a>}", 1)
        , ("{{<ab>},{<ab>},{<ab>},{<ab>}}", 9)
        , ("{{<!!>},{<!!>},{<!!>},{<!!>}}", 9)
        , ("{{<a!>},{<a!>},{<a!>},{<ab>}}", 3) ]

part2Tests :: TestTree
part2Tests = Tasty.testGroup "part 2 tests" $ do
    (i, (text, score)) <- zip @Int [1..] examples
    let parsed = Parse.runParser (Day09.parseGarbage <* Parse.eof) "test" text
    pure $ HUnit.testCase [qq|example $i|] $
        second Day09.noCancelGarbage parsed @?= Right score
  where
    examples =
        [ ([q|<>|], 0)
        , ([q|<random characters>|], 17)
        , ([q|<<<<>|], 3)
        , ([q|<{!>}>|], 2)
        , ([q|<!!>|], 0)
        , ([q|<!!!>>|], 0)
        , ([q|<{o"i!a,<{i<a>|], 10) ]
