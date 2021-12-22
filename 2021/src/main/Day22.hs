{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Day22 where

import Data.Foldable (asum, foldl')
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Flow ((.>))
import Linear (V3(V3))
import Optics ((%~), (^.), _2, _3, view)
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch
import Text.Megaparsec.Char.Lexer qualified as Par.Ch.Lex

import Common


-- types

data Setting = On | Off
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- parsing

parseSetting :: Parser Setting
parseSetting = asum
    [ On  <$ Par.Ch.string' "on"  <* Par.Ch.hspace
    , Off <$ Par.Ch.string' "off" <* Par.Ch.hspace ]

parseAxis :: Num a => Char -> Parser (a, a)
parseAxis chr = do
    Par.Ch.char' chr *> Par.Ch.hspace
    Par.Ch.char' '=' *> Par.Ch.hspace
    inf <- value <* Par.Ch.hspace
    Par.Ch.string' ".." *> Par.Ch.hspace
    sup <- value <* Par.Ch.hspace
    pure (inf, sup)
  where
    value = Par.Ch.Lex.signed Par.Ch.hspace Par.Ch.Lex.decimal

parseCuboid :: Num a => Parser (Setting, V3 a, V3 a)
parseCuboid = do
    setting <- parseSetting
    (xInf, xSup) <- parseAxis 'x'
    Par.Ch.char' ',' *> Par.Ch.hspace
    (yInf, ySup) <- parseAxis 'y'
    Par.Ch.char' ',' *> Par.Ch.hspace
    (zInf, zSup) <- parseAxis 'z'
    pure (setting, V3 xInf yInf zInf, V3 xSup ySup zSup)

parseCuboids :: Num a => Parser [(Setting, V3 a, V3 a)]
parseCuboids = parseCuboid `Par.sepEndBy` Par.Ch.space

getCuboids :: Num a => Text -> Either String [(Setting, V3 a, V3 a)]
getCuboids = runParser "day-22" $ parseCuboids <* Par.eof

-- auxilliary functions

overlaps :: Ord a => (a, a) -> (a, a) -> Bool
overlaps (inf1, sup1) (inf2, sup2) =
    not $ (inf1 > sup2) || (inf2 > sup1)

overlapsV3 :: Ord a => (V3 a, V3 a) -> (V3 a, V3 a) -> Bool
overlapsV3 (inf1, sup1) (inf2, sup2) =
    overlapsOn _x && overlapsOn _y && overlapsOn _z
  where
    overlapsOn ax = overlaps (inf1 ^. ax, sup1 ^. ax) (inf2 ^. ax, sup2 ^. ax)

clamp :: Ord a => a -> a -> a -> a
clamp inf sup = max inf .> min sup

clampV3 :: Ord a => V3 a -> V3 a -> V3 a -> V3 a
clampV3 inf sup = clampOn _x .> clampOn _y .> clampOn _z
  where
    clampOn ax = ax %~ clamp (inf ^. ax) (sup ^. ax)

-- core logic

apply :: (Ord a, Enum a) => Setting -> V3 a -> V3 a -> Set (V3 a) -> Set (V3 a)
apply setting inf sup = case setting of
    On  -> Set.union points
    Off -> flip Set.difference points
  where
    points = Set.fromList $ V3
        <$> [view _x inf .. view _x sup]
        <*> [view _y inf .. view _y sup]
        <*> [view _z inf .. view _z sup]

applyAll :: (Ord a, Enum a) => [(Setting, V3 a, V3 a)] -> Set (V3 a)
applyAll = foldl' go Set.empty
  where
    go = flip $ \(setting, inf, sup) -> apply setting inf sup

wrapWithin
    :: (Ord a, Num a)
    => a -> [(Setting, V3 a, V3 a)] -> [(Setting, V3 a, V3 a)]
wrapWithin size = filter inBounds
    .> fmap (_2 %~ clampV3 inf sup)
    .> fmap (_3 %~ clampV3 inf sup)
  where
    inBounds (_, u, v) = overlapsV3 (u, v) (inf, sup)
    inf = pure (-size)
    sup = pure size

-- parts & main

part1 :: (Num a, Ord a, Enum a) => [(Setting, V3 a, V3 a)] -> Int
part1 = wrapWithin 50 .> applyAll .> Set.size

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-22.txt"
    case getCuboids @Int text of
        Left err -> die err
        Right cuboids -> do
            print $ part1 cuboids
