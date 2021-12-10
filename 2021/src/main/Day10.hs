module Day10 where

import Control.Applicative (some, many)
import Data.Bifunctor (first)
import Data.Foldable (asum)
import Data.Functor (($>), void)
import Data.List.NonEmpty qualified as List.NE
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Void (Void)
import Flow ((.>))
import Optics (preview, _Left, _Right)
import System.Exit (die)
import Text.Megaparsec (Parsec, ParseError)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch

import Common hiding (Parser)


-- types

type Parser a b = Parsec Void a b

data Sort = Open | Close
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Kind = Paren | Square | Brace | Angled
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Delim = MkDelim
    { delimSort :: Sort
    , delimKind :: Kind
    } deriving (Eq, Ord, Read, Show)

data Chunk = MkChunk
    { chunkKind :: Kind
    , contained :: [Chunk]
    } deriving (Eq, Ord, Read, Show)

data Incomplete = MkIncomplete
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Corrupted = MkCorrupted
    { expected :: Delim
    , actual :: Delim
    } deriving (Eq, Ord, Read, Show)

-- initial parsing

parseDelim :: Parser Text Delim
parseDelim = asum
    [ Par.Ch.char '(' $> MkDelim Open Paren
    , Par.Ch.char ')' $> MkDelim Close Paren
    , Par.Ch.char '[' $> MkDelim Open Square
    , Par.Ch.char ']' $> MkDelim Close Square
    , Par.Ch.char '{' $> MkDelim Open Brace
    , Par.Ch.char '}' $> MkDelim Close Brace
    , Par.Ch.char '<' $> MkDelim Open Angled
    , Par.Ch.char '>' $> MkDelim Close Angled
    ]

parseLine :: Parser Text [Delim]
parseLine = some parseDelim

parseLines :: Parser Text [[Delim]]
parseLines = parseLine `Par.sepEndBy` Par.Ch.space

getLines :: Text -> Either String [[Delim]]
getLines = Par.parse (parseLines <* Par.eof) "day-10"
    .> first Par.errorBundlePretty

-- parsing each line

parseChunkOf :: Kind -> Parser [Delim] Chunk
parseChunkOf kind = do
    void $ Par.single $ MkDelim Open kind
    inner <- many parseChunk
    void $ Par.single $ MkDelim Close kind
    pure $ MkChunk kind inner

parseChunk :: Parser [Delim] Chunk
parseChunk = asum
    [ parseChunkOf Paren
    , parseChunkOf Square
    , parseChunkOf Brace
    , parseChunkOf Angled
    ]

parseChunks :: Parser [Delim] [Chunk]
parseChunks = some parseChunk

getChunks :: [Delim] -> Either (ParseError [Delim] Void) [Chunk]
getChunks = Par.parse parseChunks "chunks"
    .> first (Par.bundleErrors .> List.NE.head)

setMaybe :: Ord b => (a -> Maybe b) -> Set a -> Set b
setMaybe f = Set.toList .> mapMaybe f .> Set.fromList

checkParseError
    :: ParseError [Delim] Void -> Maybe (Either Incomplete Corrupted)
checkParseError = \case
    Par.TrivialError _pos mUnexpected expecteds -> case mUnexpected of
        Just Par.EndOfInput -> Just $ Left MkIncomplete
        Just (Par.Tokens unexpected) -> do
            let tokens = setMaybe getTokens expecteds
            let closes = Set.filter (delimSort .> (==) Close) tokens
            closeToken <- listToMaybe $ Set.toList closes
            pure $ Right $ MkCorrupted
                { expected = closeToken
                , actual = List.NE.head unexpected
                }
        Just Par.Label{} -> Nothing
        Nothing -> Nothing
    Par.FancyError{} -> Nothing
  where
    getTokens = \case
        Par.Tokens toks -> Just $ List.NE.head toks
        _               -> Nothing

-- corrupted lines

scoreCorrupted :: Corrupted -> Int
scoreCorrupted = actual .> delimKind .> \case
    Paren  -> 3
    Square -> 57
    Brace  -> 1197
    Angled -> 25137

part1 :: [[Delim]] -> Int
part1 = mapMaybe scoreLine .> sum
  where
    scoreLine line = do
        parseErr <- preview _Left $ getChunks line
        errType <- checkParseError parseErr
        scoreCorrupted <$> preview _Right errType

-- main

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-10.txt"
    case getLines text of
        Left err -> die err
        Right linesOfDelims -> do
            print $ part1 linesOfDelims
