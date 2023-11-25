{-# LANGUAGE LambdaCase #-}

module Day05 where

import Control.Applicative (some)
import Data.Char  qualified as Char
import Data.Containers.ListUtils (nubOrd)
import Data.Foldable qualified as Fold
import Data.Ord (comparing)
import Data.Sequence (Seq((:<|), (:|>)))
import Data.Sequence qualified as Seq
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
reactFully = fromText .> go Seq.empty .> toText
  where
    go left right = case (left, right) of
        (ls :|> l, r :<| rs) -> if reactive l r
            then go ls rs
            else go (left :|> r) rs
        (Seq.Empty, r :<| rs) -> go (Seq.singleton r) rs
        (ls, Seq.Empty) -> ls
    fromText = Text.unpack .> Seq.fromList
    toText = Fold.toList .> Text.pack

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

units :: Text -> [Char]
units = Text.toLower .> Text.unpack .> nubOrd

without :: Char -> Text -> Text
without unit = Text.filter $ \chr -> Char.toLower chr /= Char.toLower unit

improve :: Text -> Text
improve poly = case fullyReact <$> units poly of
    []     -> poly
    u : us -> infimumBy (comparing Text.length) u us
  where
    fullyReact unit = reactFully $ without unit poly

part2 :: Text -> Int
part2 = improve .> Text.length

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-05.txt"
    case getInput text of
        Left err -> die err
        Right input -> do
            print $ part1 input
            print $ part2 input
