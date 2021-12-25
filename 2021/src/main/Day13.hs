{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day13 where

import Control.Arrow ((&&&))
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable (asum, foldl')
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Linear (V2(V2))
import Optics (view, (&))
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch
import Text.Megaparsec.Char.Lexer qualified as Par.Ch.Lex

import Common
import Common.Optics


-- types

data Axis = X | Y
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Fold a = MkFold
    { axis :: Axis
    , line :: a
    } deriving (Eq, Ord, Read, Show)

-- parsing

parsePoint :: Num a => Parser (V2 a)
parsePoint = do
    x <- Par.Ch.Lex.decimal <* Par.Ch.hspace
    Par.Ch.char ',' *> Par.Ch.hspace
    y <- Par.Ch.Lex.decimal <* Par.Ch.hspace
    pure $ V2 x y

parsePoints :: Num a => Parser [V2 a]
parsePoints = parsePoint `Par.sepEndBy` Par.Ch.space

parseAxis :: Parser Axis
parseAxis = asum
    [ X <$ Par.Ch.char' 'x'
    , Y <$ Par.Ch.char' 'y'
    ]

parseFold :: Num a => Parser (Fold a)
parseFold = do
    Par.Ch.string' "fold" *> Par.Ch.hspace
    Par.Ch.string' "along" *> Par.Ch.hspace
    axis <- parseAxis <* Par.Ch.hspace
    Par.Ch.char '=' *> Par.Ch.hspace
    line <- Par.Ch.Lex.decimal <* Par.Ch.hspace
    pure $ MkFold{..}

parseFolds :: Num a => Parser [Fold a]
parseFolds = parseFold `Par.sepEndBy` Par.Ch.space

parseInput :: Num a => Parser ([V2 a], [Fold a])
parseInput = (,) <$> parsePoints <*> parseFolds

getInput :: Num a => Text -> Either String ([V2 a], [Fold a])
getInput = runParser "day-13" $ parseInput <* Par.eof

-- pretty printing

prettyGrid :: (Ord a, Enum a) => Char -> Char -> [V2 a] -> Text
prettyGrid yes no points
    | null points = ""
    | otherwise   = Text.intercalate "\n" $ do
        y <- [yMin .. yMax]
        pure $ Text.pack $ do
            x <- [xMin .. xMax]
            pure $ if V2 x y `Set.member` pointsSet then yes else no
  where
    (xMin, xMax) = (minimum &&& maximum) (view _x <$> points)
    (yMin, yMax) = (minimum &&& maximum) (view _y <$> points)
    pointsSet = Set.fromList points

-- core logic

foldAlong :: (Num a, Ord a) => Fold a -> V2 a -> V2 a
foldAlong MkFold{..} pt@(V2 x y) = case axis of
    X -> if x > line then V2 (2*line - x) y else pt
    Y -> if y > line then V2 x (2*line - y) else pt

applyFolds :: (Num a, Ord a) => [Fold a] -> [V2 a] -> [V2 a]
applyFolds = flip $ foldl' reduce
  where
    reduce points fold = nubOrd $ fmap (foldAlong fold) points

-- main

part1 :: (Ord a, Num a) => [V2 a] -> [Fold a] -> Int
part1 points folds = length $ applyFolds (take 1 folds) points

part2 :: (Ord a, Num a, Enum a) => [V2 a] -> [Fold a] -> Text
part2 points folds = prettyGrid '█' ' ' $ applyFolds folds points

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-13.txt"
    case getInput @Int text of
        Left err -> die err
        Right (points, folds) -> do
            part1 points folds & print
            part2 points folds & Text.IO.putStrLn
