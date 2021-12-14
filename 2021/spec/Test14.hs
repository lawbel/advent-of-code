{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Test14 where

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Map.NonEmpty qualified as Map.NE
import Data.Sequence.NonEmpty (NESeq)
import Data.Sequence.NonEmpty qualified as Seq.NE
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.InterpolatedString.Perl6 (qq)

import Day14 (Rule)
import Day14 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [exampleTests]

exampleTests :: TestTree
exampleTests = Tasty.testGroup "example"
    [ applyRulesNTests
    , lengthTests
    , countTests
    ]

applyRulesNTests :: TestTree
applyRulesNTests = Tasty.testGroup "applyRulesNTests" $ do
    (i, result) <- zip [1..]
        [ Seq.NE.fromList $ 'N' :| "CNBCHB"
        , Seq.NE.fromList $ 'N' :| "BCCNBBBCBHCB"
        , Seq.NE.fromList $ 'N' :| "BBBCNCCNBBNBNBBCHBHHBCHB"
        , Seq.NE.fromList $
            'N' :| "BBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"
        ]
    pure $ HUnit.testCase [qq|after $i steps|] $
        Day14.applyRulesN i exampleRules exampleTemplate @?= result

lengthTests :: TestTree
lengthTests = Tasty.testGroup "length" $ do
    (n, result) <- [(5, 97), (10, 3073)]
    pure $ HUnit.testCase [qq|after $n steps|] $
        length (Day14.applyRulesN n exampleRules exampleTemplate) @?= result

countTests :: TestTree
countTests = HUnit.testCase "count" $
    Day14.count (Day14.applyRulesN 10 exampleRules exampleTemplate)
        @?= result
  where
    result = Map.NE.fromList $
          ('B', 1749) :|
        [ ('C', 298)
        , ('H', 161)
        , ('N', 865)
        ]

exampleTemplate :: NESeq Char
exampleTemplate = Seq.NE.fromList $ 'N' :| "NCB"

exampleRules :: [Rule Char]
exampleRules =
    [ mkRule ('C', 'H') 'B'
    , mkRule ('H', 'H') 'N'
    , mkRule ('C', 'B') 'H'
    , mkRule ('N', 'H') 'C'
    , mkRule ('H', 'B') 'C'
    , mkRule ('H', 'C') 'B'
    , mkRule ('H', 'N') 'C'
    , mkRule ('N', 'N') 'C'
    , mkRule ('B', 'H') 'H'
    , mkRule ('N', 'C') 'B'
    , mkRule ('N', 'B') 'B'
    , mkRule ('B', 'N') 'B'
    , mkRule ('B', 'B') 'N'
    , mkRule ('B', 'C') 'B'
    , mkRule ('C', 'C') 'N'
    , mkRule ('C', 'N') 'C'
    ]
  where
    mkRule (start, end) insert = Day14.MkRule{..}
