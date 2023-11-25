{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day08 where

import Control.Applicative (some)
import Data.Foldable (asum)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Flow ((.>))
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch

import Common


-- types

data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Segment = A | B | C | D | E | F | G
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Entry = MkEntry
    { signals :: [Set Segment]
    , outputs :: [Set Segment]
    } deriving (Eq, Ord, Read, Show)

-- parsing

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
    signals <- parseSegments `Par.sepEndBy` Par.Ch.hspace
    Par.Ch.char '|' *> Par.Ch.hspace
    outputs <- parseSegments `Par.sepEndBy` Par.Ch.hspace
    pure $ MkEntry{..}

parseEntries :: Parser [Entry]
parseEntries = parseEntry `Par.sepEndBy` Par.Ch.space

getEntries :: Text -> Either String [Entry]
getEntries = runParser "day-08" parseEntries

-- shorthand operators for set operations

(\\) :: Ord a => Set a -> Set a -> Set a
xs \\ ys = xs Set.\\ ys

(/\) :: Ord a => Set a -> Set a -> Set a
xs /\ ys = xs `Set.intersection` ys

(\/) :: Ord a => Set a -> Set a -> Set a
xs \/ ys = xs `Set.union` ys

(-<) :: Ord a => Set a -> Set a -> Bool
xs -< ys = xs `Set.isSubsetOf` ys

(>-) :: Ord a => Set a -> Set a -> Bool
xs >- ys = ys -< xs

-- auxiliary functions

digitToInt :: Digit -> Int
digitToInt = fromEnum

numBase :: Num a => a -> [a] -> a
numBase base = reverse .> zipWith (*) [base ^ n | n :: Int <- [0..]] .> sum

single :: [a] -> Maybe a
single = \case
    [x] -> Just x
    _   -> Nothing

inverseMap :: (Bounded a, Enum a, Ord b) => (a -> b) -> Map b a
inverseMap f = Map.fromList $ do
    x <- [minBound .. maxBound]
    pure (f x, x)

-- main logic

identifyByCount :: Set Segment -> Maybe Digit
identifyByCount segments
    | size == 2 = Just D1
    | size == 4 = Just D4
    | size == 3 = Just D7
    | size == 7 = Just D8
    | otherwise = Nothing
  where
    size = Set.size segments

identify :: [Set Segment] -> Maybe (Digit -> Set Segment)
identify segments = do
    one   <- unique $ \s -> Set.size s == 2
    four  <- unique $ \s -> Set.size s == 4
    seven <- unique $ \s -> Set.size s == 3
    eight <- unique $ \s -> Set.size s == 7
    six   <- unique $ \s -> Set.size s == 6 && Set.size (s /\ one) == 1
    nine  <- unique $ \s -> Set.size s == 6 && s >- four
    zero  <- unique $ \s -> Set.size s == 6 && s >- (eight \\ four \/ one)
    three <- unique $ \s -> Set.size s == 5 && s >- seven
    five  <- unique $ \s -> Set.size s == 5 && s >- (four \\ one)
    two   <- unique $ \s -> Set.size s == 5 && s >- (eight \\ seven \\ four)
    pure $ \case
        D0 -> zero;  D1 -> one;  D2 -> two;    D3 -> three;  D4 -> four
        D5 -> five;  D6 -> six;  D7 -> seven;  D8 -> eight;  D9 -> nine
  where
    unique f = single $ filter f segments

identifyEntry :: Entry -> Maybe Int
identifyEntry MkEntry{..} = do
    assoc <- inverseMap <$> identify signals
    digits <- traverse (assoc Map.!?) outputs
    pure $ numBase 10 $ fmap digitToInt digits

part1 :: [Entry] -> Int
part1 = concatMap (outputs .> mapMaybe identifyByCount) .> length

part2 :: [Entry] -> Int
part2 = mapMaybe identifyEntry .> sum

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-08.txt"
    case getEntries text of
        Left err -> die err
        Right entries -> do
            print $ part1 entries
            print $ part2 entries
