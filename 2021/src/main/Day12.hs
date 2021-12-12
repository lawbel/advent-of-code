{-# LANGUAGE OverloadedStrings #-}

module Day12 where

import Control.Applicative (some)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Flow ((.>))
import Optics ((&), (.~), (?~), at)
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch

import Common


data Usage
    = Infinite
    | Count Int
    deriving (Eq, Ord, Show)

parseNode :: Parser Text
parseNode = Text.pack <$> some Par.Ch.letterChar

parseLine :: Parser (Text, Text)
parseLine = do
    start <- parseNode <* Par.Ch.char '-'
    end <- parseNode
    pure (start, end)

parseLines :: Parser [(Text, Text)]
parseLines = parseLine `Par.sepEndBy` Par.Ch.space

getLines :: Text -> Either String [(Text, Text)]
getLines = runParser "day-12" $ parseLines <* Par.eof

use :: Ord a => a -> Map a Usage -> Maybe (Map a Usage)
use x assoc = case assoc Map.!? x of
    Just Infinite           -> Just assoc
    Just (Count n) | n > 1  -> Just $ assoc & at x ?~ Count (n - 1)
    Just (Count n) | n == 1 -> Just $ assoc & at x .~ Nothing
    _                       -> Nothing

connections :: Ord a => [(a, a)] -> Map a [a]
connections assoc = Map.fromList $ do
    val <- Set.toList vals
    let xs = mapMaybe (startOrEnd val) assoc
    pure (val, xs)
  where
    vals = Set.fromList $ uncurry mappend $ unzip assoc
    startOrEnd x (y, z)
        | x == y    = Just z
        | x == z    = Just y
        | otherwise = Nothing

tally :: Ord a => (a -> b) -> [(a, a)] -> Map a b
tally usage assoc = Map.fromList $ do
    val <- Set.toList vals
    pure (val, usage val)
  where
    vals = Set.fromList $ uncurry mappend $ unzip assoc

pathsBetween :: Ord a => a -> a -> Map a [a] -> Map a Usage -> [[a]]
pathsBetween start end edges = flip go []
  where
    go usage = \case
        [] -> do
            next <- maybeToList $ use start usage
            go next [start]
        path@(from : _) -> if from == end
            then pure $ reverse path
            else do
                choice <- Map.findWithDefault [] from edges
                next <- maybeToList $ use choice usage
                go next (choice : path)

isLower :: Text -> Bool
isLower text = text == Text.toLower text

makePathsBetween :: Text -> Text -> [(Text, Text)] -> [[Text]]
makePathsBetween start end assoc =
    pathsBetween start end (connections assoc) (tally count assoc)
  where
    count text = if isLower text then Count 1 else Infinite

part1 :: [(Text, Text)] -> Int
part1 = makePathsBetween "start" "end" .> length

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-12.txt"
    case getLines text of
        Left err -> die err
        Right assoc -> do
            print $ part1 assoc
