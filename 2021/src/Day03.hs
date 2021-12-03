{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

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
getReports = runParser (parseReport <* Par.eof) "day-03"

duomap :: Bifunctor f => (a -> b) -> f a a -> f b b
duomap f = bimap f f

ifSetBit :: Bits a => Bool -> a -> Int -> a
ifSetBit bool x i = if bool then Bits.setBit x i else x

highestBitSet :: FiniteBits a => a -> Int
highestBitSet x = Bits.finiteBitSize x - Bits.countLeadingZeros x - 1

countBitsSet :: FiniteBits a => [a] -> [BitCount]
countBitsSet = \case
    [] -> []
    list@(x : xs) -> do
        let high = foldr max (highestBitSet x) (highestBitSet <$> xs)
        bit <- [0 .. high]
        let (set, unset) = List.partition (`Bits.testBit` bit) list
        pure $ MkBitCount
            { zeroes = length unset
            , ones = length set
            }

gammaEpsilon :: FiniteBits a => [a] -> Maybe (a, a)
gammaEpsilon list = do
    bools <- for (countBitsSet list) $ \MkBitCount{..} ->
        case zeroes `compare` ones of
            LT -> Just (True, False)
            EQ -> Nothing
            GT -> Just (False, True)
    pure $ duomap setBits $ unzip bools
  where
    setBits = zip [0..] .> foldr setBit Bits.zeroBits
    setBit (i, bool) bits = ifSetBit bool bits i

part1 :: (FiniteBits a, Num a) => [a] -> Maybe a
part1 xs = do
    (gamma, epsilon) <- gammaEpsilon xs
    pure $ gamma * epsilon

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-03.txt"
    case getReports @Int text of
        Left err -> die err
        Right report -> do
            putStrLn $ maybe "-" show $ part1 report
