{-# LANGUAGE RecordWildCards #-}

module Day02 where

import Control.Arrow ((>>>))
import Data.Foldable (asum)
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import System.Exit (die)
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.Char

import Common (Parser, readInputFileUtf8)


data Opponent = A | B | C
    deriving (Bounded, Enum, Eq, Ord, Show)

data Player = X | Y | Z
    deriving (Bounded, Enum, Eq, Ord, Show)

data Round = MkRound
    { opponent :: Opponent
    , player :: Player }
    deriving (Eq, Ord, Show)

data Outcome = OpponentWin | Draw | PlayerWin
    deriving (Bounded, Enum, Eq, Ord, Show)

parseOpponent :: Parser Opponent
parseOpponent = asum
    [ A <$ Parse.single 'A'
    , B <$ Parse.single 'B'
    , C <$ Parse.single 'C' ]

parsePlayer :: Parser Player
parsePlayer = asum
    [ X <$ Parse.single 'X'
    , Y <$ Parse.single 'Y'
    , Z <$ Parse.single 'Z' ]

parseRound :: Parser Round
parseRound = do
    opponent <- parseOpponent <* Parse.Char.hspace
    player <- parsePlayer
    pure $ MkRound{..}

parseGuide :: Parser (Vector Round)
parseGuide = Vec.fromList <$> Parse.sepEndBy parseRound Parse.Char.eol

-- | Thinking of each move as being in Z mod 3, we calculate whether the
-- player's move is equal to:
--
--   * opponent's move + 1 => player win
--   * opponent's move     => draw
--   * opponent's move - 1 => opponent win
outcome :: Round -> Outcome
outcome MkRound{..} = case shifted `compare` 0 of
    GT -> PlayerWin
    EQ -> Draw
    LT -> OpponentWin
  where
    diff = fromEnum player - fromEnum opponent
    shifted = mod (diff + 1) 3 - 1

scorePlayer :: Player -> Int
scorePlayer play = fromEnum play + 1

scoreOutcome :: Outcome -> Int
scoreOutcome out = fromEnum out * 3

scoreRound :: Round -> Int
scoreRound rnd = scorePlayer (player rnd) + scoreOutcome (outcome rnd)

part1 :: Vector Round -> Int
part1 = Vec.map scoreRound >>> Vec.sum

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-02.txt"
    case Parse.runParser (parseGuide  <* Parse.eof) "day-02" text of
        Left err -> die $ Parse.errorBundlePretty err
        Right guide -> do
            print $ part1 guide
