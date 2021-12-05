{-# LANGUAGE OverloadedStrings #-}

module Day05 where

import Control.Arrow ((&&&))
import Data.Either (partitionEithers)
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

-- | The meaning of a return value @Just (base, off)@ is that:
--
-- * @off == Left l@ implies a horizontal line - add @l@ to x
-- * @off == Right r@ implies a vertical line - add @r@ to y
straightLine :: (Num a, Ord a) => V2 a -> V2 a -> Maybe (V2 a, Either a a)
straightLine v1@(V2 x1 y1) v2@(V2 x2 y2)
    | (y1 == y2) && (x2 >  x1) = Just (v1, Left  $ x2 - x1)
    | (y1 == y2) && (x2 <= x1) = Just (v2, Left  $ x1 - x2)
    | (x1 == x2) && (y2 >  y1) = Just (v1, Right $ y2 - y1)
    | (x1 == x2) && (y2 <= y1) = Just (v2, Right $ y1 - y2)
    | otherwise                = Nothing

-- | The meaning of a return value @Just (base, off)@ is that:
--
-- * @off == Left l@ implies a /positive/ gradient - add @l@ to x and y
-- * @off == Right r@ implies a /negative/ gradient - add @r@ to x, subtract
--   it from y
diagonalLine :: (Num a, Ord a) => V2 a -> V2 a -> Maybe (V2 a, Either a a)
diagonalLine v1 v2
    | (x3 ==  y3) && (x3 >  0) = Just (v1, Left x3)
    | (x3 ==  y3) && (x3 <= 0) = Just (v2, Left $ -x3)
    | (x3 == -y3) && (x3 >  0) = Just (v1, Right x3)
    | (x3 == -y3) && (x3 <= 0) = Just (v2, Right $ -x3)
    | otherwise                = Nothing
  where
    V2 x3 y3 = v2 - v1

plotStraightLine :: Integral a => V2 a -> Either a a -> Map (V2 a) Int
plotStraightLine vec = \case
    Left x -> Map.fromList $ do
        off <- [0 .. x]
        pure (vec & _x +~ off, 1)
    Right y -> Map.fromList $ do
        off <- [0 .. y]
        pure (vec & _y +~ off, 1)

plotDiagonalLine :: Integral a => V2 a -> Either a a -> Map (V2 a) Int
plotDiagonalLine vec = \case
    Left l -> Map.fromList $ do
        off <- [0 .. l]
        pure (vec + V2 off off, 1)
    Right r -> Map.fromList $ do
        off <- [0 .. r]
        pure (vec + V2 off (-off), 1)

plotStraightLines :: Integral a => [(V2 a, Either a a)] -> Map (V2 a) Int
plotStraightLines =
    fmap (uncurry plotStraightLine)
        .> foldr (Map.unionWith (+)) Map.empty

plotDiagonalLines :: Integral a => [(V2 a, Either a a)] -> Map (V2 a) Int
plotDiagonalLines =
    fmap (uncurry plotDiagonalLine)
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

part1 :: Integral a => [(V2 a, V2 a)] -> Int
part1 = mapMaybe (uncurry straightLine)
    .> plotStraightLines
    .> Map.filter (>= 2)
    .> length

maybeEither :: (a -> Maybe b) -> (a -> Maybe c) -> a -> Maybe (Either b c)
maybeEither left right x = case left x of
    Just l  -> Just $ Left l
    Nothing -> case right x of
        Just r  -> Just $ Right r
        Nothing -> Nothing

plotLines :: Integral a => [(V2 a, V2 a)] -> Map (V2 a) Int
plotLines vecs = Map.unionWith (+)
    (plotStraightLines straights)
    (plotDiagonalLines diagonals)
  where
    tryLine = maybeEither (uncurry straightLine) (uncurry diagonalLine)
    (straights, diagonals) = partitionEithers $ mapMaybe tryLine vecs

part2 :: Integral a => [(V2 a, V2 a)] -> Int
part2 = plotLines .> Map.filter (>= 2) .> length

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-05.txt"
    case getLines @Int text of
        Left err -> die err
        Right lines' -> do
            print $ part1 lines'
            print $ part2 lines'
