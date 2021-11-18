{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day01 where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch
import Text.Megaparsec.Char.Lexer qualified as Par.Ch.Lex

import Common


parseFreqChange :: Parser Int
parseFreqChange = Par.Ch.Lex.signed Par.Ch.space Par.Ch.Lex.decimal

parseFreqChangesCommas :: Parser [Int]
parseFreqChangesCommas =
    parseFreqChange `Par.sepBy` (Par.Ch.char ',' *> Par.Ch.space)

parseFreqChangesLines :: Parser [Int]
parseFreqChangesLines = parseFreqChange `Par.sepEndBy` Par.Ch.newline

part1 :: Text -> Either String Int
part1 input = do
    changes <- runParser parseFreqChangesLines "day-01" input
    pure $ sum changes

partialSums :: Num a => [a] -> [a]
partialSums = scanl (+) 0

firstRepeat :: forall a. Ord a => [a] -> Maybe a
firstRepeat = go Set.empty
  where
    go :: Set a -> [a] -> Maybe a
    go seen = \case
        [] -> Nothing
        x : xs -> if x `Set.member` seen
            then Just x
            else go (Set.insert x seen) xs

part2 :: Text -> Either String Int
part2 input = do
    changes <- runParser parseFreqChangesLines "day-01" input
    let loop = partialSums $ cycle changes
    maybeToEither "no repeats" $ firstRepeat loop

main :: IO ()
main = do
    input <- readInputFileUtf8 "input/day-01.txt"
    print $ part1 input
    print $ part2 input
