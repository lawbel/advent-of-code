{-# LANGUAGE OverloadedStrings #-}

module Day01 where

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

main :: IO ()
main = do
    input <- readInputFileUtf8 "input/day-01.txt"
    print $ part1 input
