{-# LANGUAGE RecordWildCards #-}

module Day08 where

import Control.Applicative (Alternative, empty)
import Control.Monad (replicateM, (>=>))
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Void (Void)
import Flow ((.>))
import System.Exit (die)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch
import Text.Megaparsec.Char.Lexer qualified as Par.Ch.Lex

import Common hiding (Parser, runParser)

type Parser a b = Parsec Void a b

data Header = MkHeader
    { numChildren :: Int
    , numMetadata :: Int
    }
    deriving (Eq, Ord, Show, Read)

data Tree a = Node
    { header :: Header
    , children :: [Tree a]
    , metadata :: NonEmpty a
    }
    deriving (Eq, Ord, Show, Read)

replicateM1 :: Alternative f => Int -> f a -> f (NonEmpty a)
replicateM1 n action
    | n <= 0 = empty
    | otherwise = (:|) <$> action <*> replicateM (n - 1) action

parseVals :: Parser Text [Int]
parseVals = Par.Ch.Lex.decimal `Par.sepEndBy` Par.Ch.space

parseHeader :: Parser [Int] Header
parseHeader = do
    numChildren <- Par.anySingle
    numMetadata <- Par.anySingle
    pure $ MkHeader{..}

parseTree :: Parser [Int] (Tree Int)
parseTree = do
    header <- parseHeader
    children <- replicateM (numChildren header) parseTree
    metadata <- replicateM1 (numMetadata header) Par.anySingle
    pure $ Node{..}

getInput :: Text -> Either String (Tree Int)
getInput = getVals >=> getTree
  where
    getVals =
        Par.parse (parseVals <* Par.eof) "day-08"
            .> first Par.errorBundlePretty
    getTree =
        Par.parseMaybe (parseTree <* Par.eof)
            .> maybeToEither "couldn't parse tree"

sumOfMetadata :: Num a => Tree a -> a
sumOfMetadata Node{..} = sum metadata + sum (sumOfMetadata <$> children)

part1 :: Num a => Tree a -> a
part1 = sumOfMetadata

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-08.txt"
    case getInput text of
        Left err -> die err
        Right input -> do
            print $ part1 input
