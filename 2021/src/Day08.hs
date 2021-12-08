module Day08 where

import Control.Applicative (some)
import Data.Foldable (asum)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Flow ((.>))
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch

import Common


data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Segment = A | B | C | D | E | F | G
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Entry = MkEntry
    { signalPatterns :: [Set Segment]
    , outputValues :: [Set Segment]
    } deriving (Eq, Ord, Read, Show)

parseSegment :: Parser Segment
parseSegment = asum
    [ A <$ Par.Ch.char 'a'
    , B <$ Par.Ch.char 'b'
    , C <$ Par.Ch.char 'c'
    , D <$ Par.Ch.char 'd'
    , E <$ Par.Ch.char 'e'
    , F <$ Par.Ch.char 'f'
    , G <$ Par.Ch.char 'g'
    ]

parseSegments :: Parser (Set Segment)
parseSegments = Set.fromList <$> some parseSegment

parseEntry :: Parser Entry
parseEntry = do
    signal <- parseSegments `Par.sepEndBy` Par.Ch.hspace
    Par.Ch.char '|' *> Par.Ch.hspace
    output <- parseSegments `Par.sepEndBy` Par.Ch.hspace
    pure $ MkEntry
        { signalPatterns = signal
        , outputValues = output
        }

parseEntries :: Parser [Entry]
parseEntries = parseEntry `Par.sepEndBy` Par.Ch.space

getEntries :: Text -> Either String [Entry]
getEntries = runParser "day-08" parseEntries

identifyByCount :: Set Segment -> Maybe Digit
identifyByCount segments
    | size == 2 = Just D1
    | size == 4 = Just D4
    | size == 3 = Just D7
    | size == 7 = Just D8
    | otherwise = Nothing
  where
    size = Set.size segments

part1 :: [Entry] -> Int
part1 = concatMap (outputValues .> mapMaybe identifyByCount) .> length

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-08.txt"
    case getEntries text of
        Left err -> die err
        Right entries -> do
            print $ part1 entries
