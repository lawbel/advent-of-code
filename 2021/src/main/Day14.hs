{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Day14 where

import Control.Applicative (some)
import Data.Foldable qualified as Fold
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Sequence.NonEmpty qualified as Seq.NE
import Data.Text (Text)
import Flow ((.>))
import Optics ((&))
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

parseTemplate :: Parser (Seq Char)
parseTemplate = Seq.fromList <$> some Par.Ch.letterChar

parseRule :: Parser (Rule Char)
parseRule = do
    start <- Par.Ch.letterChar <* Par.Ch.hspace
    end <- Par.Ch.letterChar <* Par.Ch.hspace
    Par.Ch.string "->" *> Par.Ch.hspace
    insert <- Par.Ch.letterChar <* Par.Ch.hspace
    pure $ MkRule{..}

parseInput :: Parser (Seq Char, [Rule Char])
parseInput = do
    template <- parseTemplate <* Par.Ch.space
    rules <- parseRule `Par.sepEndBy` Par.Ch.space
    pure (template, rules)

getInput :: Text -> Either String (Seq Char, [Rule Char])
getInput = runParser "day-14" $ parseInput <* Par.eof

-- naive approach

asPairs :: Foldable f => f a -> Seq (a, a)
asPairs vals = Seq.fromList $ zip list (drop 1 list)
  where
    list = Fold.toList vals

applyRule :: Eq a => Rule a -> Seq a -> Seq a
applyRule MkRule{..} vals =
    foldr (\i -> Seq.insertAt (i + 1) insert) vals hits
  where
    hits = Seq.elemIndicesR (start, end) (asPairs vals)

applyRules :: Eq a => [Rule a] -> Seq a -> Seq a
applyRules rules vals =
    foldr (\(i, ins) -> Seq.insertAt (i + 1) ins) vals hits
  where
    matches MkRule{..} =
        (, insert) <$> Seq.elemIndicesR (start, end) (asPairs vals)
    hits = List.sortOn fst $ concatMap matches rules

applyRulesN :: Eq a => Int -> [Rule a] -> Seq a -> Seq a
applyRulesN n rules = compose n $ applyRules rules

count :: (Foldable f, Ord a) => f a -> Map a Int
count = Fold.toList .> fmap (, 1) .> Map.fromListWith (+)

mostCommon :: Ord a => Seq a -> Maybe Int
mostCommon = count .> mostCommonIn

leastCommon :: Ord a => Seq a -> Maybe Int
leastCommon = count .> leastCommonIn

-- using counters

asCount :: Ord a => Seq a -> Map (a, a) Int
asCount = asPairs .> Fold.toList .> fmap (, 1) .> Map.fromListWith (+)

countRule :: Ord a => Rule a -> Map (a, a) Int -> Map (a, a) Int
countRule MkRule{..} counter = case counter Map.!? (start, end) of
    Nothing -> counter
    Just n  -> counter
        & Map.delete (start, end)
        & Map.insertWith (+) (start, insert) n
        & Map.insertWith (+) (insert, end) n

countRules :: Ord a => [Rule a] -> Map (a, a) Int -> Map (a, a) Int
countRules rules counter = counter
    & Map.unionWith (+) matches
    & Map.filter (/= 0)
  where
    matches = Map.fromListWith (+) $ concatMap match rules
    match MkRule{..} = case counter Map.!? (start, end) of
        Nothing -> []
        Just n  ->
            [ ((start, end), -n)
            , ((start, insert), n)
            , ((insert, end), n)
            ]

countRulesN :: Ord a => Int -> [Rule a] -> Map (a, a) Int -> Map (a, a) Int
countRulesN n rules = compose n $ countRules rules

countPairs :: Ord a => Map (a, a) Int -> Map a Int
countPairs counter = Map.unionWith (+) starts ends
  where
    starts = Map.mapKeysWith (+) fst counter
    ends = Map.mapKeysWith (+) snd counter

countApplyRulesN :: Ord a => Int -> [Rule a] -> Seq a -> Map a Int
countApplyRulesN n rules template = fmap (`div` 2) counter
  where
    counter = Map.unionsWith (+)
        [ countPairs $ countRulesN n rules $ asCount template
        , makeMap $ Seq.take 1 template
        , makeMap $ Seq.take 1 $ Seq.reverse template
        ]
    makeMap = fmap (, 1) .> Fold.toList .> Map.fromList

mostCommonIn :: Map a Int -> Maybe Int
mostCommonIn = Map.elems
    .> Seq.fromList
    .> Seq.NE.withNonEmpty Nothing (supremum1 .> Just)

leastCommonIn :: Map a Int -> Maybe Int
leastCommonIn = Map.elems
    .> Seq.fromList
    .> Seq.NE.withNonEmpty Nothing (infimum1 .> Just)

-- main hook

part1 :: Ord a => Seq a -> [Rule a] -> Maybe Int
part1 template rules = (-) <$> mostCommon poly <*> leastCommon poly
  where
    poly = applyRulesN 10 rules template

part2 :: Ord a => Seq a -> [Rule a] -> Maybe Int
part2 template rules = (-) <$> mostCommonIn poly <*> leastCommonIn poly
  where
    poly = countApplyRulesN 40 rules template

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-14.txt"
    case getInput text of
        Left err -> die err
        Right (template, rules) -> do
            part1 template rules & maybe "???" show & putStrLn
            part2 template rules & maybe "???" show & putStrLn
