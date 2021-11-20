{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test03 where

import Data.Containers.ListUtils (nubOrd)
import Flow ((.>))
import Linear (V2(V2))
import Numeric.Interval ((...))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as Tasty.HUnit
import Test.Tasty.QuickCheck (Arbitrary, arbitrary)
import Test.Tasty.QuickCheck qualified as Tasty.Check
import Text.Megaparsec qualified as Par

import Day03 (Claim)
import Day03 qualified


instance (Arbitrary a, Ord a, Num a) => Arbitrary (Claim a) where
    arbitrary = do
        Tasty.Check.Positive idNum <- arbitrary
        Tasty.Check.NonNegative baseX <- arbitrary
        Tasty.Check.NonNegative baseY <- arbitrary
        Tasty.Check.Positive sizeX <- arbitrary
        Tasty.Check.Positive sizeY <- arbitrary
        let base = fromIntegral @Int <$> V2 baseX baseY
        let size = fromIntegral @Int <$> V2 sizeX sizeY
        pure $ Day03.MkClaim{..}

main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [propertyTests, parseTests, unitTests]

propertyTests :: TestTree
propertyTests = Tasty.testGroup "properties"
    [ Tasty.Check.testProperty "prettyClaim .> parseClaim == Just" $
        \(claim :: Claim Int) ->
            let roundTrip =
                    Day03.prettyClaim .> Par.parseMaybe Day03.parseClaim
            in  roundTrip claim == Just claim
    , Tasty.Check.testProperty "groupConflicts works" $
        \(claims :: [Claim Float]) ->
            let naive = Day03.conflicts .> Day03.overlaps .> length
                optimized = Day03.conflicts
                    .> Day03.groupConflicts
                    .> fmap Day03.overlaps
                    .> concat
                    .> nubOrd
                    .> length
            in  naive claims == optimized claims
    ]

exOne :: Claim Int
exOne = Day03.MkClaim{idNum = 1, base = V2 1 3, size = V2 4 4}

exTwo :: Claim Int
exTwo = Day03.MkClaim{idNum = 2, base = V2 3 1, size = V2 4 4}

exThree :: Claim Int
exThree = Day03.MkClaim{idNum = 3, base = V2 5 5, size = V2 2 2}

parseTests :: TestTree
parseTests = Tasty.testGroup "parse tests"
    [ Tasty.HUnit.testCase "example 1" $
        Par.parseMaybe Day03.parseClaim "#1 @ 1,3: 4x4" @?= Just exOne
    , Tasty.HUnit.testCase "example 2" $
        Par.parseMaybe Day03.parseClaim "#2 @ 3,1: 4x4" @?= Just exTwo
    , Tasty.HUnit.testCase "example 3" $
        Par.parseMaybe Day03.parseClaim "#3 @ 5,5: 2x2" @?= Just exThree
    ]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests"
    [ Tasty.HUnit.testCase "example 1" $
        Day03.claimToBox exOne `Day03.boxIntersect` Day03.claimToBox exTwo
            @?= overlap
    , Tasty.HUnit.testCase "example 2" $
        Day03.claimToBox exOne `disjoint` Day03.claimToBox exThree @?= True
    , Tasty.HUnit.testCase "example 3" $
        Day03.claimToBox exTwo `disjoint` Day03.claimToBox exThree @?= True
    , Tasty.HUnit.testCase "example 4" $
        Day03.conflicts [exOne, exTwo, exThree] @?= [overlap]
    ]
  where
    overlap = Day03.MkBox{xAxis = 3 ... 5, yAxis = 3 ... 5}
    disjoint x y = Day03.boxArea (x `Day03.boxIntersect` y) == 0
