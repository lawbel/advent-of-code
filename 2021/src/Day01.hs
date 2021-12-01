{-# LANGUAGE TypeApplications #-}

module Day01 where

import Data.Text (Text)
import Flow ((.>))
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch
import Text.Megaparsec.Char.Lexer qualified as Par.Ch.Lex

import Common


parseDepth :: Num a => Parser a
parseDepth = Par.Ch.Lex.decimal

parseDepths :: Num a => Parser [a]
parseDepths = Par.Ch.Lex.decimal `Par.sepEndBy` Par.Ch.newline

getDepths :: Num a => Text -> Either String [a]
getDepths = runParser (parseDepths <* Par.eof) "day-01"

increases :: Ord a => [a] -> [a]
increases = \case
    []     -> []
    x : xs -> reverse $ go [] x xs
  where
    go acc prev = \case
        []     -> acc
        y : ys -> go (if y > prev then y : acc else acc) y ys

part1 :: Ord a => [a] -> Int
part1 = increases .> length

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-01.txt"
    case getDepths @Int text of
        Left err -> die err
        Right depths -> do
            print $ part1 depths
