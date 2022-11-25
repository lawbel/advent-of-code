{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Day12 where

import Common (readInputFileUtf8)
import Data.Hashable (Hashable)
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Text (Text)
import Data.Void (Void)
import Flow ((.>))
import System.Exit (die)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.Char
import Text.Megaparsec.Char.Lexer qualified as Parse.Lex


type Parser = Parsec Void Text

newtype ID = MkID { unID :: Int }
    deriving (Eq, Ord, Show)
    deriving Hashable via Int

data Pipe = MkPipe
    { inputID :: ID
    , outputIDs :: HashSet ID }
    deriving (Eq, Ord, Show)

parsePipe :: Parser Pipe
parsePipe = do
    input <- fmap MkID Parse.Lex.decimal <* Parse.Char.hspace
    Parse.chunk "<->" *> Parse.Char.hspace
    outputs <- fmap MkID Parse.Lex.decimal `Parse.sepEndBy` comma
    pure $ MkPipe
        { inputID = input
        , outputIDs = HashSet.fromList outputs }
  where
    comma = Parse.single ',' *> Parse.Char.hspace

parsePipes :: Parser [Pipe]
parsePipes = parsePipe `Parse.sepEndBy` Parse.Char.eol

connected :: ID -> [Pipe] -> HashSet ID
connected m pipes = go m (HashSet.singleton m)
  where
    go :: ID -> HashSet ID -> HashSet ID
    go n soFar = soFar `HashSet.union` HashSet.foldl' add HashSet.empty nexts
      where
        naive = direct n
        nexts = naive `HashSet.difference` soFar
        add acc next = acc `HashSet.union` go next (HashSet.insert next soFar)

    direct :: ID -> HashSet ID
    direct n = HashSet.delete n $ HashSet.unions $ map outputIDs $
        filter (inputID .> (==) n) pipes

part1 :: [Pipe] -> Int
part1 = connected (MkID 0) .> HashSet.size

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-12.txt"
    case Parse.runParser parsePipes "day 12" text of
        Left err -> die $ Parse.errorBundlePretty err
        Right pipes -> do
            print $ part1 pipes
