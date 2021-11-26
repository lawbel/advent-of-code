{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day07 where

import Algebra.Graph (Graph)
import Algebra.Graph qualified as Graph
import Algebra.Graph.ToGraph qualified as Graph.To
import Data.Foldable (traverse_)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Flow ((.>))
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch

import Common


data Instr a = MkInstr
    { before :: a
    , after :: a
    } deriving (Eq, Ord, Read, Show)

parseInstr :: Parser (Instr Char)
parseInstr = do
    string "step"
    before <- Par.Ch.letterChar <* Par.Ch.hspace
    string "must be finished before step"
    after <- Par.Ch.letterChar <* Par.Ch.hspace
    string "can begin."
    pure $ MkInstr{..}
  where
    string = Text.words .> traverse_ (\w -> Par.Ch.string' w <* Par.Ch.hspace)

parseInstrs :: Parser [Instr Char]
parseInstrs = parseInstr `Par.sepEndBy` Par.Ch.newline

getInput :: Text -> Either String [Instr Char]
getInput = runParser (parseInstrs <* Par.eof) "day-07"

instrsToGraph :: [Instr a] -> Graph a
instrsToGraph = fmap toArc .> Graph.edges
  where
    toArc MkInstr{..} = (before, after)

available :: Ord a => Graph a -> [a]
available graph = filter nullPreSet $ Graph.vertexList graph
  where
    nullPreSet = flip Graph.To.preSet graph .> Set.null

order :: Ord a => Graph a -> Maybe [a]
order graph
    | Graph.isEmpty graph = Just []
    | x : xs <- available graph =
        let inf = infimum x xs
        in  (inf :) <$> order (Graph.removeVertex inf graph)
    | otherwise = Nothing

part1 :: [Instr Char] -> Either String [Char]
part1 = instrsToGraph .> order .> maybeToEither "no coherent order"

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-07.txt"
    case getInput text of
        Left err -> die err
        Right input -> do
            print $ part1 input
