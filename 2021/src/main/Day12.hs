{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Day12 where

import Control.Applicative (some)
import Data.Containers.ListUtils (nubOrd)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as Text
import Flow ((.>))
import Optics ((&), (.~), (?~), (%), (%~), at, makePrisms, ix)
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch

import Common


data Usage
    = Infinite
    | Count Int
    deriving (Eq, Ord, Show)

makePrisms ''Usage

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
    Just Infinite          -> Just assoc
    Just (Count n) | n > 1 -> Just $ assoc & at x ?~ Count (n - 1)
    Just (Count 1)         -> Just $ assoc & at x .~ Nothing
    _                      -> Nothing

useGiven :: Ord a => Int -> a -> Map a Usage -> [(Int, Map a Usage)]
useGiven i x assoc = case assoc Map.!? x of
    Just Infinite                   -> [(i, assoc)]
    Just (Count n) | n > 1 && i > 0 -> [(j, assoc), (i, decr)]
    Just (Count n) | n > 1          -> [(i, decr)]
    Just (Count 1) | i > 0          -> [(j, assoc), (i, del)]
    Just (Count 1)                  -> [(i, del)]
    _                               -> []
  where
    decr = assoc & ix x % _Count %~ subtract 1
    del = assoc & at x .~ Nothing
    j = i - 1

connections :: Ord a => [(a, a)] -> Map a [a]
connections assoc = Map.fromList $ do
    val <- nubOrd $ uncurry mappend $ unzip assoc
    pure (val, mapMaybe (startOrEnd val) assoc)
  where
    startOrEnd x (y, z)
        | x == y    = Just z
        | x == z    = Just y
        | otherwise = Nothing

tally :: Ord a => (a -> b) -> [(a, a)] -> Map a b
tally usage assoc = Map.fromList $ do
    val <- nubOrd $ uncurry mappend $ unzip assoc
    pure (val, usage val)

pathsBetween :: Ord a => a -> a -> Map a [a] -> Map a Usage -> [[a]]
pathsBetween = pathsBetweenGiven 0

pathsBetweenGiven
    :: Ord a => Int -> a -> a -> Map a [a] -> Map a Usage -> [[a]]
pathsBetweenGiven extra start end edges = flip (go extra) [] .> nubOrd
  where
    go x usage = \case
        [] -> do
            (y, next) <- useExcept x start usage
            go y next [start]
        path@(from : _) -> if from == end
            then pure $ reverse path
            else do
                choice <- Map.findWithDefault [] from edges
                (y, next) <- useExcept x choice usage
                go y next (choice : path)
    useExcept x node usage
        | node `elem` [start, end] = (x, ) <$> maybeToList (use node usage)
        | otherwise                = useGiven x node usage

isLower :: Text -> Bool
isLower text = text == Text.toLower text

makePathsBetween :: Text -> Text -> [(Text, Text)] -> [[Text]]
makePathsBetween = makePathsBetweenGiven 0

makePathsBetweenGiven :: Int -> Text -> Text -> [(Text, Text)] -> [[Text]]
makePathsBetweenGiven n start end assoc =
    pathsBetweenGiven n start end (connections assoc) (tally count assoc)
  where
    count text = if isLower text then Count 1 else Infinite

part1 :: [(Text, Text)] -> Int
part1 = makePathsBetween "start" "end" .> length

part2 :: [(Text, Text)] -> Int
part2 = makePathsBetweenGiven 1 "start" "end" .> length

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-12.txt"
    case getLines text of
        Left err -> die err
        Right assoc -> do
            print $ part1 assoc
            print $ part2 assoc
