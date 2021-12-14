{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Day14 where

import Control.Applicative.Combinators.NonEmpty qualified as App.NE
import Data.List qualified as List
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as Map.NE
import Data.Ord (comparing)
import Data.Semigroup.Foldable qualified as Fold1
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty (NESeq)
import Data.Sequence.NonEmpty qualified as Seq.NE
import Data.Text (Text)
import Flow ((.>))
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch

import Common


-- types

data Rule a = MkRule
    { start :: a
    , end :: a
    , insert :: a
    } deriving (Eq, Ord, Read, Show)

-- parsing

parseTemplate :: Parser (NESeq Char)
parseTemplate = Seq.NE.fromList <$> App.NE.some Par.Ch.letterChar

parseRule :: Parser (Rule Char)
parseRule = do
    start <- Par.Ch.letterChar <* Par.Ch.hspace
    end <- Par.Ch.letterChar <* Par.Ch.hspace
    Par.Ch.string "->" *> Par.Ch.hspace
    insert <- Par.Ch.letterChar <* Par.Ch.hspace
    pure $ MkRule{..}

parseInput :: Parser (NESeq Char, [Rule Char])
parseInput = do
    template <- parseTemplate <* Par.Ch.space
    rules <- parseRule `Par.sepEndBy` Par.Ch.space
    pure (template, rules)

getInput :: Text -> Either String (NESeq Char, [Rule Char])
getInput = runParser "day-14" $ parseInput <* Par.eof

-- core logic

applyRule :: Eq a => Rule a -> NESeq a -> NESeq a
applyRule MkRule{..} vals =
    foldr (\i -> Seq.NE.insertAt (i + 1) insert) vals hits
  where
    pairs = Seq.zip (Seq.NE.toSeq vals) (Seq.NE.drop 1 vals)
    hits = Seq.elemIndicesR (start, end) pairs

applyRules :: Eq a => [Rule a] -> NESeq a -> NESeq a
applyRules rules vals =
    foldr (\(i, ins) -> Seq.NE.insertAt (i + 1) ins) vals hits
  where
    pairs = Seq.zip (Seq.NE.toSeq vals) (Seq.NE.drop 1 vals)
    matches MkRule{..} = (, insert) <$> Seq.elemIndicesR (start, end) pairs
    hits = List.sortOn fst $ concatMap matches rules

applyRulesN :: Eq a => Int -> [Rule a] -> NESeq a -> NESeq a
applyRulesN n rules = compose n $ applyRules rules

count :: Ord a => NESeq a -> NEMap a Int
count = fmap (, 1) .> Fold1.toNonEmpty .> Map.NE.fromListWith (+)

mostCommon :: Ord a => NESeq a -> (a, Int)
mostCommon = count .> Map.NE.toList .> supremumBy1 (comparing snd)

leastCommon :: Ord a => NESeq a -> (a, Int)
leastCommon = count .> Map.NE.toList .> infimumBy1 (comparing snd)

-- main hook

part1 :: Ord a => NESeq a -> [Rule a] -> Int
part1 template rules = snd (mostCommon poly) - snd (leastCommon poly)
  where
    poly = applyRulesN 10 rules template

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-14.txt"
    case getInput text of
        Left err -> die err
        Right (template, rules) -> do
            print $ part1 template rules
