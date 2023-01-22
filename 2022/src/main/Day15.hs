{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Day15 where

import Control.Arrow ((>>>))
import Data.ExtendedReal (Extended(Finite))
import Data.Interval (Interval, (<=..<=))
import Data.Interval qualified as Interval
import Data.IntervalSet (IntervalSet)
import Data.IntervalSet qualified as IntervalSet
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import GHC.Generics (Generic)
import Linear (V2(V2))
import Optics ((^.), (%), view)
import System.Exit (die)
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.C
import Text.Megaparsec.Char.Lexer qualified as Parse.CL

import Common (Parser, _x, _y, readInputFileUtf8)


data Reading = MkReading
    { sensorPos :: V2 Int
    , closestBeacon :: V2 Int }
    deriving (Eq, Generic, Ord, Show)

parseInt :: Parser Int
parseInt = Parse.CL.signed Parse.C.hspace Parse.CL.decimal

parseReading :: Parser Reading
parseReading = do
    xSensor <- Parse.chunk "Sensor at x=" *> parseInt
    ySensor <- Parse.chunk ", y=" *> parseInt
    xBeacon <- Parse.chunk ": closest beacon is at x=" *> parseInt
    yBeacon <- Parse.chunk ", y=" *> parseInt
    pure $ MkReading
        { sensorPos = V2 xSensor ySensor
        , closestBeacon = V2 xBeacon yBeacon }

parseReadings :: Parser (Vector Reading)
parseReadings = Vec.fromList <$> Parse.sepEndBy parseReading Parse.C.eol


distFromBeacon :: Reading -> Int
distFromBeacon r = sum $ abs $ closestBeacon r - sensorPos r

rowCantHaveBeacon :: Int -> Reading -> Maybe (Interval Int)
rowCantHaveBeacon row reading
    | radius < 0 = Nothing
    | row /= view (#closestBeacon % _y) reading = Just $
        Finite (x - radius) <=..<= Finite (x + radius)
    | x < view (#closestBeacon % _x) reading = Just $
        Finite (x - radius) <=..<= Finite (x + radius - 1)
    | otherwise = Just $
        Finite (x - radius + 1) <=..<= Finite (x + radius)
  where
    radius = distFromBeacon reading - abs (row - y)
    x = reading ^. #sensorPos % _x
    y = reading ^. #sensorPos % _y

rowCantHaveBeacons :: Foldable t => Int -> t Reading -> IntervalSet Int
rowCantHaveBeacons row = foldMap $
    rowCantHaveBeacon row >>> maybe IntervalSet.empty IntervalSet.singleton


rowCantHaveBeaconNaive :: Int -> Reading -> Set (V2 Int)
rowCantHaveBeaconNaive y reading
    | abs delta > dist = Set.empty
    | delta >= 0 = removeBeacon $ Set.fromList $ do
        offset <- [delta - dist .. dist - delta]
        pure $ V2 (x + offset) y
    | otherwise = removeBeacon $ Set.fromList $ do
        offset <- [-delta - dist .. dist + delta]
        pure $ V2 (x + offset) y
  where
    delta = y - view (#sensorPos % _y) reading
    dist = distFromBeacon reading
    x = reading ^. #sensorPos % _x
    removeBeacon = Set.delete $ closestBeacon reading

rowCantHaveBeaconsNaive :: Foldable t => Int -> t Reading -> Set (V2 Int)
rowCantHaveBeaconsNaive row = foldMap $ rowCantHaveBeaconNaive row


part1 :: Vector Reading -> Int
part1 = rowCantHaveBeacons 2_000_000
    >>> IntervalSet.toList
    >>> fmap (Interval.width >>> (+) 1)
    >>> sum

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-15.txt"
    case Parse.runParser (parseReadings <* Parse.eof) "day-15" text of
        Left err -> die $ Parse.errorBundlePretty err
        Right readings -> do
            print $ part1 readings
