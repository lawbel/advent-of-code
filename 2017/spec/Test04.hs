{-# LANGUAGE OverloadedStrings #-}

module Test04 where

import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.InterpolatedString.Perl6 (qq)

import Day04 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [part1Tests]

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests" $ do
    (i, text, valid) <- zipWith shuffle [1..]
        [ ("aa bb cc dd ee", True)
        , ("aa bb cc dd aa", False)
        , ("aa bb cc dd aaa", True) ]
    pure $ HUnit.testCase [qq|example $i|] $
        Day04.valid text @?= valid
  where
    shuffle :: Int -> (a, b) -> (Int, a, b)
    shuffle i (x, y) = (i, x, y)
