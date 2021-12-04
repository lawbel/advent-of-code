{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Day03 where

import Data.Bifunctor (Bifunctor, bimap)
import Data.Bits (FiniteBits, Bits)
import Data.Bits qualified as Bits
import Data.List qualified as List
import Data.Text (Text)
import Data.Traversable (for)
import Flow ((.>))
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch
import Text.Megaparsec.Char.Lexer qualified as Par.Ch.Lex

import Common


data BitCount = MkBitCount
    { zeroes :: Int
    , ones :: Int
    } deriving (Eq, Ord, Read, Show)

parseReport :: Num a => Parser [a]
parseReport = Par.Ch.Lex.binary `Par.sepEndBy` Par.Ch.space

getReports :: Num a => Text -> Either String [a]
getReports = runParser "day-03" (parseReport <* Par.eof)

duomap :: Bifunctor f => (a -> b) -> f a a -> f b b
duomap f = bimap f f

ifSetBit :: Bits a => Bool -> a -> Int -> a
ifSetBit bool x i = if bool then Bits.setBit x i else x

highestBitSet :: FiniteBits a => a -> Int
highestBitSet x = Bits.finiteBitSize x - Bits.countLeadingZeros x - 1

highestBitSetIn :: FiniteBits a => [a] -> Int
highestBitSetIn = \case
    []     -> 0
    x : xs -> foldr max (highestBitSet x) (highestBitSet <$> xs)

countBitsAt :: FiniteBits a => Int -> [a] -> BitCount
countBitsAt i list = MkBitCount
    { zeroes = length unset
    , ones = length set
    }
  where
    (set, unset) = List.partition (`Bits.testBit` i) list

countBits :: FiniteBits a => [a] -> [BitCount]
countBits list = do
    let high = highestBitSetIn list
    bit <- [0 .. high]
    pure $ countBitsAt bit list

gammaEpsilon :: FiniteBits a => [a] -> Maybe (a, a)
gammaEpsilon list = do
    bools <- for (countBits list) $ \MkBitCount{..} ->
        case zeroes `compare` ones of
            LT -> Just (True, False)
            EQ -> Nothing
            GT -> Just (False, True)
    pure $ duomap setBits $ unzip bools
  where
    setBits = zip [0..] .> foldr setBit Bits.zeroBits
    setBit (i, bool) bits = ifSetBit bool bits i

scrub :: FiniteBits a => (Int -> [a] -> a -> Bool) -> [a] -> Maybe a
scrub criteria list = go (highestBitSetIn list) list
  where
    go i = \case
        []  -> Nothing
        [x] -> Just x
        xs  -> go (i - 1) $ filter (criteria i xs) xs

oxygen :: FiniteBits a => [a] -> Maybe a
oxygen = scrub $ \i list ->
    let MkBitCount{..} = countBitsAt i list
    in  (`Bits.testBit` i) .> (if zeroes <= ones then id else not)

co2 :: FiniteBits a => [a] -> Maybe a
co2 = scrub $ \i list ->
    let MkBitCount{..} = countBitsAt i list
    in  (`Bits.testBit` i) .> (if zeroes <= ones then not else id)

part1 :: (FiniteBits a, Num a) => [a] -> Maybe a
part1 list = uncurry (*) <$> gammaEpsilon list

part2 :: (FiniteBits a, Num a) => [a] -> Maybe a
part2 list = (*) <$> oxygen list <*> co2 list

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-03.txt"
    case getReports @Int text of
        Left err -> die err
        Right report -> do
            putStrLn $ maybe "???" show $ part1 report
            putStrLn $ maybe "???" show $ part2 report
