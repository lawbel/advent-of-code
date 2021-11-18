{-# LANGUAGE OverloadedStrings #-}

module Day01 where

-- import Data.Text (Text)
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

main :: IO ()
main = do
    print $ runParser parseFreqChangesCommas "day-01" "+1, -2, +3, +1"
