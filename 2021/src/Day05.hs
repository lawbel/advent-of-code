{-# LANGUAGE OverloadedStrings #-}

module Day05 where

import Control.Arrow ((&&&))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Flow ((.>))
import Linear (V2(V2))
import Optics ((&), view)
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch
import Text.Megaparsec.Char.Lexer qualified as Par.Ch.Lex

import Common


parsePoint :: Num a => Parser (V2 a)
parsePoint = do
    x <- Par.Ch.Lex.decimal <* Par.Ch.char ','
    y <- Par.Ch.Lex.decimal <* Par.Ch.hspace
    pure $ V2 x y

parseLine :: Num a => Parser (V2 a, V2 a)
parseLine = do
    u <- parsePoint
    Par.Ch.string "->" *> Par.Ch.hspace
    v <- parsePoint
    pure (u, v)

parseLines :: Num a => Parser [(V2 a, V2 a)]
parseLines = parseLine `Par.sepEndBy` Par.Ch.space

getLines :: Num a => Text -> Either String [(V2 a, V2 a)]
getLines = runParser "day-05" $ parseLines <* Par.eof

cartesianLine :: (Num a, Ord a) => V2 a -> V2 a -> Maybe (V2 a, Either a a)
cartesianLine v1@(V2 x1 y1) v2@(V2 x2 y2)
    | (y1 == y2) && (x2 >  x1) = Just (v1, Left  $ x2 - x1)
    | (y1 == y2) && (x2 <= x1) = Just (v2, Left  $ x1 - x2)
    | (x1 == x2) && (y2 >  y1) = Just (v1, Right $ y2 - y1)
    | (x1 == x2) && (y2 <= y1) = Just (v2, Right $ y1 - y2)
    | otherwise                = Nothing

plotCartesianLine :: Integral a => V2 a -> Either a a -> Map (V2 a) Int
plotCartesianLine vec = \case
    Left x -> Map.fromList $ do
        off <- [0 .. x]
        pure (vec & _x +~ off, 1)
    Right y -> Map.fromList $ do
        off <- [0 .. y]
        pure (vec & _y +~ off, 1)

plotCartesianLines :: Integral a => [(V2 a, Either a a)] -> Map (V2 a) Int
plotCartesianLines =
    fmap (uncurry plotCartesianLine)
        .> foldr (Map.unionWith (+)) Map.empty

prettyGrid :: (Enum a, Ord a) => Map (V2 a) Int -> Text
prettyGrid lattice
    | null lattice = ""
    | otherwise    = Text.intercalate "\n" $ do
        y <- [yMin .. yMax]
        pure $ join $ do
            x <- [xMin .. xMax]
            let count = maybe "." textShow $ Map.lookup (V2 x y) lattice
            pure $ Text.justifyLeft width ' ' count
  where
    (xMin, xMax) = (minimum &&& maximum) (view _x <$> Map.keys lattice)
    (yMin, yMax) = (minimum &&& maximum) (view _y <$> Map.keys lattice)
    width = maximum $ fmap (textShow .> Text.length) $ Map.elems lattice
    join = if width > 1 then Text.unwords else mconcat

numPointsWith :: Integral a => (Int -> Bool) -> [(V2 a, V2 a)] -> Int
numPointsWith f = mapMaybe (uncurry cartesianLine)
    .> plotCartesianLines
    .> Map.filter f
    .> length

part1 :: Integral a => [(V2 a, V2 a)] -> Int
part1 = numPointsWith (>= 2)

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-05.txt"
    case getLines @Int text of
        Left err -> die err
        Right lines' -> do
            print $ part1 lines'
