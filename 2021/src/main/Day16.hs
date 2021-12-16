{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day16 where

import Control.Monad (replicateM)
import Control.Monad.Combinators.NonEmpty qualified as Monad.NE
import Data.Bifunctor (first)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as List.NE
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Flow ((.>))
import Language.Haskell.TH.Syntax (Lift)
import System.Exit (die)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch

import Common hiding (Parser)


-- types

type Parser from to = Parsec Void from to

data Bit = B0 | B1
    deriving (Bounded, Enum, Eq, Lift, Ord, Read, Show)

data Packet a
    = PacketL (Literal a)
    | PacketO (Operator a)
    deriving (Eq, Foldable, Functor, Lift, Ord, Read, Show)

data Literal a = MkLiteral
    { litVersion :: a
    , litTypeID :: a
    , litValue :: a }
    deriving (Eq, Foldable, Functor, Lift, Ord, Read, Show)

data Operator a = MkOperator
    { opVersion :: a
    , opTypeID :: a
    , opLenTypeId :: a
    , opLength :: a
    , opPackets :: NonEmpty (Packet a) }
    deriving (Eq, Foldable, Functor, Lift, Ord, Read, Show)

-- parsing Text to [Bit]

parseHexChar :: Parser Text [Bit]
parseHexChar = asum
    [ Par.Ch.char' '0' $> [B0, B0, B0, B0]
    , Par.Ch.char' '1' $> [B0, B0, B0, B1]
    , Par.Ch.char' '2' $> [B0, B0, B1, B0]
    , Par.Ch.char' '3' $> [B0, B0, B1, B1]
    , Par.Ch.char' '4' $> [B0, B1, B0, B0]
    , Par.Ch.char' '5' $> [B0, B1, B0, B1]
    , Par.Ch.char' '6' $> [B0, B1, B1, B0]
    , Par.Ch.char' '7' $> [B0, B1, B1, B1]
    , Par.Ch.char' '8' $> [B1, B0, B0, B0]
    , Par.Ch.char' '9' $> [B1, B0, B0, B1]
    , Par.Ch.char' 'A' $> [B1, B0, B1, B0]
    , Par.Ch.char' 'B' $> [B1, B0, B1, B1]
    , Par.Ch.char' 'C' $> [B1, B1, B0, B0]
    , Par.Ch.char' 'D' $> [B1, B1, B0, B1]
    , Par.Ch.char' 'E' $> [B1, B1, B1, B0]
    , Par.Ch.char' 'F' $> [B1, B1, B1, B1] ]

parseHexChars :: Parser Text [Bit]
parseHexChars = concat <$> Par.some parseHexChar

-- parsing [Bit] to Packet [Bit]

parsePacket :: Parser [Bit] (Packet [Bit])
parsePacket = do
    version <- replicateM 3 Par.anySingle
    typeID <- replicateM 3 Par.anySingle
    case bitsToInt typeID of
        4 -> PacketL <$> parseLiteral version typeID
        _ -> PacketO <$> parseOperator version typeID

parseLiteral :: [Bit] -> [Bit] -> Parser [Bit] (Literal [Bit])
parseLiteral version typeId = do
    value <- parseValue
    pure $ MkLiteral
        { litVersion = version
        , litTypeID = typeId
        , litValue = value }

parseValue :: Parser [Bit] [Bit]
parseValue = replicateM 5 Par.anySingle >>= \case
    B1 : bs -> mappend bs <$> parseValue
    B0 : bs -> pure bs
    _       -> fail "parseValue: impossible"

parseOperator :: [Bit] -> [Bit] -> Parser [Bit] (Operator [Bit])
parseOperator version typeId = Par.anySingle >>= \case
    lenType@B1 -> do
        len <- replicateM 11 Par.anySingle
        packets <- replicateM (bitsToInt len) parsePacket
        case List.NE.nonEmpty packets of
            Nothing -> fail "parseOperator: zero sub-packets for operator"
            Just packetsNE -> pure $ MkOperator
                { opVersion = version
                , opTypeID = typeId
                , opLenTypeId = [lenType]
                , opLength = len
                , opPackets = packetsNE }
    lenType@B0 -> do
        len <- replicateM 15 Par.anySingle
        bits <- replicateM (bitsToInt len) Par.anySingle
        case Par.parseMaybe (Monad.NE.some parsePacket) bits of
            Nothing -> fail "parseOperator: sub-packets don't fit in length"
            Just packets -> pure $ MkOperator
                { opVersion = version
                , opTypeID = typeId
                , opLenTypeId = [lenType]
                , opLength = len
                , opPackets = packets }

getPacket :: Text -> Either String (Packet [Bit])
getPacket text = do
    bits <- first Par.errorBundlePretty $
        Par.parse (parseHexChars <* Par.Ch.space <* Par.eof) "day-16" text
    maybeToEither "couldn't parse packet from bits" $
        Par.parseMaybe (parsePacket' <* Par.eof) bits
  where
    parsePacket' = parsePacket <* Par.many (Par.single B0)

-- pretty printing

prettyBit :: Bit -> Char
prettyBit = \case
    B0 -> '0'
    B1 -> '1'

prettyBits :: [Bit] -> Text
prettyBits = fmap prettyBit .> Text.pack

-- helper functions

bitToInt :: Bit -> Int
bitToInt = \case
    B0 -> 0
    B1 -> 1

bitsToInt :: [Bit] -> Int
bitsToInt = fmap bitToInt
    .> reverse
    .> zipWith (*) [ 2^n | n :: Int <- [0..] ]
    .> sum

-- core logic

sumOfVersions :: Num a => Packet a -> a
sumOfVersions = \case
    PacketL lit -> litVersion lit
    PacketO op  -> opVersion op + sum (sumOfVersions <$> opPackets op)

part1 :: Packet [Bit] -> Int
part1 = fmap bitsToInt .> sumOfVersions

-- main hook

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-16.txt"
    case getPacket text of
        Left err -> die err
        Right packet -> do
            print $ part1 packet
