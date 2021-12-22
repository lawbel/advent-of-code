{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Day22 where

import Data.Foldable (asum, foldl')
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Flow ((.>))
import Linear (V3(V3))
import Optics ((%~), (^.), (%), _2, view)
import Optics.TH (makeFieldLabelsWith, noPrefixFieldLabels)
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch
import Text.Megaparsec.Char.Lexer qualified as Par.Ch.Lex

import Common


-- types

data Setting = On | Off
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Cuboid a = MkCuboid
    { lower :: V3 a
    , upper :: V3 a }
    deriving (Eq, Ord, Read, Show)

makeFieldLabelsWith noPrefixFieldLabels ''Cuboid

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

parseCuboid :: Num a => Parser (Setting, Cuboid a)
parseCuboid = do
    setting <- parseSetting
    (xInf, xSup) <- parseAxis 'x'
    Par.Ch.char' ',' *> Par.Ch.hspace
    (yInf, ySup) <- parseAxis 'y'
    Par.Ch.char' ',' *> Par.Ch.hspace
    (zInf, zSup) <- parseAxis 'z'
    let lower = V3 xInf yInf zInf
    let upper = V3 xSup ySup zSup
    pure (setting, MkCuboid{..})

parseCuboids :: Num a => Parser [(Setting, Cuboid a)]
parseCuboids = parseCuboid `Par.sepEndBy` Par.Ch.space

getCuboids :: Num a => Text -> Either String [(Setting, Cuboid a)]
getCuboids = runParser "day-22" $ parseCuboids <* Par.eof

-- auxilliary functions

overlaps :: Ord a => (a, a) -> (a, a) -> Bool
overlaps (inf1, sup1) (inf2, sup2) = not $ (inf1 > sup2) || (inf2 > sup1)

overlaps' :: Ord a => Cuboid a -> Cuboid a -> Bool
overlaps' cub1 cub2 = overlapsOn _x && overlapsOn _y && overlapsOn _z
  where
    bounds val ax = (val ^. #lower % ax, val ^. #upper % ax)
    overlapsOn ax = bounds cub1 ax `overlaps` bounds cub2 ax

clamp :: Ord a => a -> a -> a -> a
clamp inf sup = max inf .> min sup

clamp' :: Ord a => Cuboid a -> V3 a -> V3 a
clamp' MkCuboid{..} = clampOn _x .> clampOn _y .> clampOn _z
  where
    clampOn ax = ax %~ clamp (lower ^. ax) (upper ^. ax)

-- core logic

apply :: (Ord a, Enum a) => Setting -> Cuboid a -> Set (V3 a) -> Set (V3 a)
apply setting MkCuboid{..} = case setting of
    On  -> Set.union points
    Off -> flip Set.difference points
  where
    points = Set.fromList $ V3
        <$> [view _x lower .. view _x upper]
        <*> [view _y lower .. view _y upper]
        <*> [view _z lower .. view _z upper]

applyAll :: (Ord a, Enum a) => [(Setting, Cuboid a)] -> Set (V3 a)
applyAll = foldl' (flip $ uncurry apply) Set.empty

wrapWithin
    :: (Ord a, Num a) => a -> [(Setting, Cuboid a)] -> [(Setting, Cuboid a)]
wrapWithin size = filter (snd .> overlaps' cuboid)
    .> fmap (_2 % #lower %~ clamp' cuboid)
    .> fmap (_2 % #upper %~ clamp' cuboid)
  where
    cuboid = MkCuboid
        { lower = pure (-size)
        , upper = pure size }

-- parts & main

part1 :: (Num a, Ord a, Enum a) => [(Setting, Cuboid a)] -> Int
part1 = wrapWithin 50 .> applyAll .> Set.size

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-22.txt"
    case getCuboids @Int text of
        Left err -> die err
        Right cuboids -> do
            print $ part1 cuboids
