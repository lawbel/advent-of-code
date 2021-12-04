{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Day02 where

import Control.Monad.State qualified as State
import Data.Foldable (asum, foldl')
import Data.Text (Text)
import Optics ((&), use)
import Optics.TH (noPrefixFieldLabels, makeFieldLabelsWith)
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch
import Text.Megaparsec.Char.Lexer qualified as Par.Ch.Lex

import Common


data Position a = MkPosition
    { horiz :: a
    , depth :: a
    , aim :: a
    } deriving (Eq, Ord, Read, Show)

makeFieldLabelsWith noPrefixFieldLabels ''Position

data Command a
    = Forward a
    | Down a
    | Up a
    deriving (Eq, Ord, Read, Show)

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
getCommands = runParser "day-02" (parseCommands <* Par.eof)

run1 :: Num a => Command a -> Position a -> Position a
run1 = \case
    Forward x -> #horiz +~ x
    Down x    -> #depth +~ x
    Up x      -> #depth -~ x

run2 :: Num a => Command a -> Position a -> Position a
run2 = \case
    Down x    -> #aim +~ x
    Up x      -> #aim -~ x
    Forward x -> State.execState $ do
        #horiz += x
        cur <- use #aim
        #depth += (cur * x)

runs1 :: Num a => [Command a] -> Position a -> Position a
runs1 cmds pos = foldl' (flip run1) pos cmds

runs2 :: Num a => [Command a] -> Position a -> Position a
runs2 cmds pos = foldl' (flip run2) pos cmds

area :: Num a => Position a -> a
area p = depth p * horiz p

part1 :: Num a => [Command a] -> a
part1 cmds = runs1 cmds (MkPosition 0 0 0) & area

part2 :: Num a => [Command a] -> a
part2 cmds = runs2 cmds (MkPosition 0 0 0) & area

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-02.txt"
    case getCommands @Int text of
        Left err -> die err
        Right commands -> do
            print $ part1 commands
            print $ part2 commands
