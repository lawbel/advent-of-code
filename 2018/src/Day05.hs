{-# LANGUAGE LambdaCase #-}

module Day05 where

import Control.Applicative (some)
import Data.Char  qualified as Char
import Data.Text (Text)
import Data.Text qualified as Text
import Flow ((.>))
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch

import Common


parsePolymer :: Parser Text
parsePolymer = do
    txt <- Text.pack <$> some Par.Ch.letterChar
    Par.Ch.space
    pure txt

getInput :: Text -> Either String Text
getInput = runParser (parsePolymer <* Par.eof) "day-05"

reactFully :: Text -> Text
reactFully txt =
    if Text.length next < Text.length txt
        then reactFully next
        else next
  where
    next = react txt

react :: Text -> Text
react = Text.unpack .> go .> Text.pack
  where
    go = \case
        x : ys@(y : zs) -> if reactive x y
            then go zs
            else x : go ys
        str -> str

trigger :: Text -> Text
trigger = Text.unpack .> go .> Text.pack
  where
    go = \case
        x : ys@(y : zs) -> if reactive x y
            then zs
            else x : go ys
        str -> str

reactive :: Char -> Char -> Bool
reactive x y = (x /= y) && (Char.toLower x == Char.toLower y)

part1 :: Text -> Int
part1 = reactFully .> Text.length

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-05.txt"
    case getInput text of
        Left err -> die err
        Right input -> do
            print $ part1 input
