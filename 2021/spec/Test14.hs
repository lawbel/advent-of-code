{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test14 where

import Data.Map qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Flow ((.>))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck (Arbitrary, (===), arbitrary)
import Test.Tasty.QuickCheck qualified as Check
import Text.InterpolatedString.Perl6 (qq)

import Day14 (Rule)
import Day14 qualified


instance Arbitrary a => Arbitrary (Rule a) where
    arbitrary = Day14.MkRule <$> arbitrary <*> arbitrary <*> arbitrary

main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests, propertyTests]

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
    (i, result) <- zip [1..] $ fmap Seq.fromList
        [ "NCNBCHB"
        , "NBCCNBBBCBHCB"
        , "NBBBCNCCNBBNBNBBCHBHHBCHB"
        , "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"
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
    result = Map.fromList
        [ ('B', 1749)
        , ('C', 298)
        , ('H', 161)
        , ('N', 865)
        ]

propertyTests :: TestTree
propertyTests = Tasty.testGroup "property tests"
    [ Check.testProperty "length applyRulesN == sum countRulesN + 1" $
        let notNull = Seq.null .> not
        in  Check.forAll (arbitrary `Check.suchThat` notNull) $
                \template rules (Check.NonNegative n) ->
                    let naive = Day14.applyRulesN n rules template
                        counted = Day14.countRulesN @Char n rules asCount
                        asCount = Day14.asCount template
                    in  length naive === sum counted + 1
    , Check.testProperty "applyRulesN .> count == countApplyRulesN" $
        \template rules (Check.NonNegative n) ->
            let naive = Day14.count $ Day14.applyRulesN n rules template
                counted = Day14.countApplyRulesN @Char n rules template
            in  naive === counted
    ]

exampleTemplate :: Seq Char
exampleTemplate = Seq.fromList "NNCB"

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
