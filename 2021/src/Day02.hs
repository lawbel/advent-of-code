{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Day02 where

import Data.Foldable (asum)
import Data.Text (Text)
import Optics ((&))
import Optics.TH (noPrefixFieldLabels, makeFieldLabelsWith)
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch
import Text.Megaparsec.Char.Lexer qualified as Par.Ch.Lex

import Common


data Position a = MkPosition
    { horiz :: a
    , depth :: a
    } deriving (Eq, Functor, Foldable, Ord, Read, Show)

makeFieldLabelsWith noPrefixFieldLabels ''Position

data Command a
    = Forward a
    | Down a
    | Up a
    deriving (Eq, Functor, Ord, Read, Show)

parseDecimal :: Num a => Text -> Parser a
parseDecimal str = do
    Par.Ch.string' str *> Par.Ch.hspace
    Par.Ch.Lex.decimal <* Par.Ch.hspace

parseCommand :: Num a => Parser (Command a)
parseCommand = asum
    [ Forward <$> parseDecimal "forward"
    , Down <$> parseDecimal "down"
    , Up <$> parseDecimal "up"
    ]

parseCommands :: Num a => Parser [Command a]
parseCommands = parseCommand `Par.sepEndBy` Par.Ch.newline

getCommands :: Num a => Text -> Either String [Command a]
getCommands = runParser (parseCommands <* Par.eof) "day-02"

run :: Num a => Command a -> Position a -> Position a
run = \case
    Forward x -> #horiz +~ x
    Down x    -> #depth +~ x
    Up x      -> #depth -~ x

runs :: Num a => [Command a] -> Position a -> Position a
runs = flip $ foldr run

part1 :: Num a => [Command a] -> a
part1 cmds = runs cmds (MkPosition 0 0) & product

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-02.txt"
    case getCommands @Int text of
        Left err -> die err
        Right commands -> do
            print $ part1 commands
