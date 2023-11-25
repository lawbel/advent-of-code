{-# LANGUAGE LambdaCase #-}

module Test15 where

import Control.Arrow ((>>>))
import Data.ExtendedReal (Extended(Finite))
import Data.Interval (Interval)
import Data.Interval qualified as Interval
import Data.IntervalSet (IntervalSet)
import Data.IntervalSet qualified as IntervalSet
import Data.Set (Set)
import Data.Set qualified as Set
import Hedgehog (Gen, (===))
import Hedgehog qualified as Hedge
import Hedgehog.Gen qualified as Hedge.G
import Hedgehog.Range qualified as Hedge.R
import Linear (V2(V2))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.Hedgehog qualified as Tasty.H
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as Tasty.U

import Day15 qualified
import Day15 (Reading)


genRow :: Gen Int
genRow = Hedge.G.int $ Hedge.R.linear 5 95

genReading :: Gen Reading
genReading = do
    sensor <- genV2 $ Hedge.G.int $ Hedge.R.linear 35 65
    beacon <- genV2 $ Hedge.G.int $ Hedge.R.linear 40 60
    pure $ Day15.MkReading
        { Day15.sensorPos = sensor
        , Day15.closestBeacon = beacon }
  where
    genV2 g = V2 <$> g <*> g

quickToNaive :: Int -> Maybe (Interval Int) -> Set (V2 Int)
quickToNaive row = \case
    Nothing -> Set.empty
    Just interval -> Set.fromList $ do
        x <- [lower interval .. upper interval]
        pure $ V2 x row

quickToNaives :: Int -> IntervalSet Int -> Set (V2 Int)
quickToNaives row intervals = Set.fromList $ do
    interval <- IntervalSet.toList intervals
    x <- [lower interval .. upper interval]
    pure $ V2 x row


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [propTests, unitTests]

propTests :: TestTree
propTests = Tasty.testGroup "property tests"
    [ Tasty.H.testProperty "rowCantHaveBeacon[Naive] agree" $
        Hedge.property $ do
            row <- Hedge.forAll genRow
            reading <- Hedge.forAll genReading
            Day15.rowCantHaveBeaconNaive row reading ===
                quickToNaive row (Day15.rowCantHaveBeacon row reading)
    , Tasty.H.testProperty "rowCantHaveBeacons[Naive] agree" $
        Hedge.property $ do
            row <- Hedge.forAll genRow
            readings <- Hedge.forAll $
                Hedge.G.list (Hedge.R.linear 1 5) genReading
            Day15.rowCantHaveBeaconsNaive row readings ===
                quickToNaives row (Day15.rowCantHaveBeacons row readings) ]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [part1Tests]

part1Tests :: TestTree
part1Tests = Tasty.testGroup "part 1 tests"
    [ Tasty.U.testCase "example" $ True @?= True ]


lower :: Interval a -> a
lower = Interval.lowerBound >>> \case
    Finite x -> x
    _        -> error "lower: not finite"

upper :: Interval a -> a
upper = Interval.upperBound >>> \case
    Finite x -> x
    _        -> error "upper: not finite"
