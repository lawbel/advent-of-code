{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test05 where

import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text qualified as Text
import Linear (V2(V2))
import Optics ((.~), (&))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck (Arbitrary, (===))
import Test.Tasty.QuickCheck qualified as Check

import Day05 qualified
import Common


instance Arbitrary a => Arbitrary (V2 a) where
    arbitrary = V2 <$> Check.arbitrary <*> Check.arbitrary

main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests"
    [ Tasty.testGroup "straight"
        [ Tasty.testGroup "straightLine"
            [ HUnit.testCase "example 1" $
                Day05.straightLine @Int (V2 1 1) (V2 1 3)
                    @?= Just (V2 1 1, Right 2)
            , HUnit.testCase "example 2" $
                Day05.straightLine @Int (V2 9 7) (V2 7 7)
                    @?= Just (V2 7 7, Left 2)
            , HUnit.testCase "example 3" $
                Day05.straightLine @Int (V2 0 0) (V2 8 8)
                    @?= Nothing
            ]
        , Tasty.testGroup "plotStraightLine"
            [ HUnit.testCase "example 1" $
                Day05.plotStraightLine @Int (V2 1 1) (Right 2)
                    @?= Map.fromList [ (V2 1 y, 1) | y <- [1..3] ]
            , HUnit.testCase "example 2" $
                Day05.plotStraightLine @Int (V2 7 7) (Left 2)
                    @?= Map.fromList [ (V2 x 7, 1) | x <- [7..9] ]
            ]
        ]
    , Tasty.testGroup "diagonal"
        [ Tasty.testGroup "diagonalLine"
            [ HUnit.testCase "example 1" $
                Day05.diagonalLine @Int (V2 1 1) (V2 3 3)
                    @?= Just (V2 1 1, Left 2)
            , HUnit.testCase "example 2" $
                Day05.diagonalLine @Int (V2 9 7) (V2 7 9)
                    @?= Just (V2 7 9, Right 2)
            , HUnit.testCase "example 3" $
                Day05.diagonalLine @Int (V2 0 0) (V2 0 8)
                    @?= Nothing
            ]
        , Tasty.testGroup "plotDiagonalLine"
            [ HUnit.testCase "example 1" $
                Day05.plotDiagonalLine @Int (V2 1 1) (Left 2)
                    @?= Map.fromList [ (V2 s s, 1) | s <- [1..3] ]
            , HUnit.testCase "example 2" $
                let result = Map.fromList $ do
                        s <- [0 .. 2]
                        pure (V2 7 9 + V2 s (-s), 1)
                in  Day05.plotDiagonalLine @Int (V2 7 9) (Right 2)
                        @?= result
            ]
        ]
    , Tasty.testGroup "prettyGrid"
        [ HUnit.testCase "plotStraightLines" $
            let actual = exampleLines
                    & mapMaybe (uncurry Day05.straightLine)
                    & Day05.plotStraightLines
                    & Day05.prettyGrid
                expected = Text.intercalate "\n"
                    [ ".......1.."
                    , "..1....1.."
                    , "..1....1.."
                    , ".......1.."
                    , ".112111211"
                    , ".........."
                    , ".........."
                    , ".........."
                    , ".........."
                    , "222111...."
                    ]
            in  actual @?= expected
        , HUnit.testCase "plotLines" $
            let actual = Day05.prettyGrid $ Day05.plotLines exampleLines
                expected = Text.intercalate "\n"
                    [ "1.1....11."
                    , ".111...2.."
                    , "..2.1.111."
                    , "...1.2.2.."
                    , ".112313211"
                    , "...1.2...."
                    , "..1...1..."
                    , ".1.....1.."
                    , "1.......1."
                    , "222111...."
                    ]
            in  actual @?= expected
        ]
    , Tasty.testGroup "overlaps"
        [ HUnit.testCase "plotStraightLines" $
            let count = exampleLines
                    & mapMaybe (uncurry Day05.straightLine)
                    & Day05.plotStraightLines
                    & Map.filter (>= 2)
                    & length
            in  count @?= 5
        , HUnit.testCase "plotLines" $
            let count = exampleLines
                    & Day05.plotLines
                    & Map.filter (>= 2)
                    & length
            in  count @?= 12
        ]
    ]

propertyTests :: TestTree
propertyTests = Tasty.testGroup "property tests"
    [ Tasty.testGroup "straightLine"
        [ Check.testProperty "commutative" $
            \u v -> Day05.straightLine @Int u v === Day05.straightLine v u
        , Check.testProperty "returns endpoints" $
            let tryRun = \(p, vec1) ->
                    let vec2 = case p of
                            Left x  -> vec1 & _x .~ x
                            Right y -> vec1 & _y .~ y
                    in  do  (base, off) <- Day05.straightLine @Int vec1 vec2
                            pure (vec1, vec2, base, off)
            in  Check.forAll (Check.arbitrary `Check.suchThatMap` tryRun) $
                    \(vec1, vec2, base, off) ->
                        let end = case off of
                                Left x  -> base + V2 x 0
                                Right y -> base + V2 0 y
                        in  List.sort [base, end] === List.sort [vec1, vec2]
        ]
    , Tasty.testGroup "diagonalLine"
        [ Check.testProperty "commutative" $
            \u v -> Day05.diagonalLine @Int u v === Day05.diagonalLine v u
        , Check.testProperty "returns endpoints" $
            let tryRun = \(p, vec1) ->
                    let vec2 = case p of
                            Left l  -> vec1 + V2 l l
                            Right r -> vec1 + V2 r (-r)
                    in  do  (base, off) <- Day05.diagonalLine @Int vec1 vec2
                            pure (vec1, vec2, base, off)
            in  Check.forAll (Check.arbitrary `Check.suchThatMap` tryRun) $
                    \(vec1, vec2, base, off) ->
                        let end = case off of
                                Left l  -> base + V2 l l
                                Right r -> base + V2 r (-r)
                        in  List.sort [base, end] === List.sort [vec1, vec2]
        ]
    ]

exampleLines :: [(V2 Int, V2 Int)]
exampleLines =
    [ (V2 0 9, V2 5 9)
    , (V2 8 0, V2 0 8)
    , (V2 9 4, V2 3 4)
    , (V2 2 2, V2 2 1)
    , (V2 7 0, V2 7 4)
    , (V2 6 4, V2 2 0)
    , (V2 0 9, V2 2 9)
    , (V2 3 4, V2 1 4)
    , (V2 0 0, V2 8 8)
    , (V2 5 5, V2 8 2)
    ]
