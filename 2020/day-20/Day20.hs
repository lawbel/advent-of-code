{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day20 where

import Control.Applicative.Combinators.NonEmpty qualified as Parse.NE
import Data.ByteString qualified as Bytes
import Data.Foldable (asum)
import Data.Function ((&), on)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence.NonEmpty (NESeq)
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Enc
import Data.Void (Void)
import Flow ((.>))
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.Char
import Text.Megaparsec.Char.Lexer qualified as Parse.Char.Lex
import Text.Pretty.Simple (pPrint)


-- types

type Parser = Parse.Parsec Void Text

data Pixel = Black | White
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Side = North | East | South | West
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Tile = MkTile
    { idNum :: Int
    , rows :: NESeq (NESeq Pixel)
    } deriving (Eq, Ord, Read, Show)

-- core functions

edge :: Tile -> Side -> NESeq Pixel
edge tile = \case
    North -> tileRows & NESeq.head
    East  -> tileRows & fmap NESeq.last
    South -> tileRows & NESeq.last
    West  -> tileRows & fmap NESeq.head
  where
    tileRows = rows tile

possibleEdges :: Tile -> Set (NESeq Pixel)
possibleEdges tile = Set.fromList (naive <> flipped)
  where
    naive = edge tile <$> allOptions
    flipped = NESeq.reverse <$> naive

isPossibleNeighbour :: Tile -> Tile -> Bool
isPossibleNeighbour = intersect `on` possibleEdges

possibleNeighboursOf :: Tile -> Set Tile -> Set Tile
possibleNeighboursOf tile = Set.filter possible
  where
    possible other = (tile `isPossibleNeighbour` other) && tile /= other

mapPossibleNeighbours :: Set Tile -> Map Tile (Set Tile)
mapPossibleNeighbours tiles =
    Map.fromSet (`possibleNeighboursOf` tiles) tiles

corners :: Set Tile -> Set Tile
corners = mapPossibleNeighbours .> Map.filter hasSize2 .> Map.keysSet
  where
    hasSize2 set = Set.size set == 2

part1 :: Set Tile -> Int
part1 = corners .> Set.map idNum .> product

-- main

main :: IO ()
main = do
    file <- readFileUtf8 "day-20/input.txt"
    case Parse.parse (parseTiles <* Parse.eof) "day 20 input" file of
        Left err -> putStrLn $ Parse.errorBundlePretty err
        Right tiles -> do
            print $ part1 tiles

-- parsers

parseTiles :: Parser (Set Tile)
parseTiles = do
    tiles <- parseTile `Parse.sepEndBy` Parse.Char.newline
    pure $ Set.fromList tiles

parseTile :: Parser Tile
parseTile = do
    Parse.Char.string "Tile" *> Parse.Char.space
    idNumber <- Parse.Char.Lex.decimal <* Parse.Char.space
    Parse.Char.char ':' *> Parse.Char.space
    tileRows <- parseRow `Parse.NE.sepEndBy1` Parse.Char.newline
    pure $ MkTile
        { idNum = idNumber
        , rows = NESeq.fromList tileRows
        }

parseRow :: Parser (NESeq Pixel)
parseRow = NESeq.fromList <$> Parse.NE.some parsePixel

parsePixel :: Parser Pixel
parsePixel = asum
    [ Black <$ Parse.Char.char '#'
    , White <$ Parse.Char.char '.'
    ]

-- misc

allOptions :: (Bounded a, Enum a) => [a]
allOptions = [minBound .. maxBound]

intersect :: Ord a => Set a -> Set a -> Bool
intersect set1 set2 = not $ Set.disjoint set1 set2

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 = Bytes.readFile .> fmap Text.Enc.decodeUtf8
