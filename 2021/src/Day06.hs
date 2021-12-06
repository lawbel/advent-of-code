{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Day06 where

import Control.Monad (replicateM_)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State (State, runState)
import Data.Text (Text)
import Flow ((.>))
import Optics ((&), use, traversed, zoomMany)
import Optics.State.Operators ((.=), (%=))
import Optics.TH (noPrefixFieldLabels, makeFieldLabelsWith)
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch
import Text.Megaparsec.Char.Lexer qualified as Par.Ch.Lex

import Common


data Fish a = MkFish
    { timer :: a
    , children :: [Fish a]
    } deriving (Eq, Foldable, Ord, Read, Show)

makeFieldLabelsWith noPrefixFieldLabels ''Fish

data Growth a = MkGrowth
    { delay :: a
    , initial :: a
    } deriving (Eq, Ord, Read, Show)

makeFieldLabelsWith noPrefixFieldLabels ''Growth

type Sea a as b = ReaderT (Growth a) (State as) b

execSea :: Growth a -> as -> Sea a as b -> as
execSea growth fish = flip runReaderT growth .> flip runState fish .> snd

parseFish :: Num a => Parser [a]
parseFish = Par.Ch.Lex.decimal `Par.sepBy` Par.Ch.char ','

getFish :: Num a => Text -> Either String [a]
getFish = runParser "day-06" $ parseFish <* Par.Ch.space <* Par.eof

grow :: Num a => Growth a
grow = MkGrowth 6 8

mkFish :: a -> Fish a
mkFish n = MkFish n []

day :: (Num a, Ord a) => Sea a [Fish a] ()
day = zoomMany traversed $ do
    zoomMany #children day
    t <- subtract 1 <$> use #timer
    if t >= 0
        then #timer .= t
        else do
            d <- seek #delay
            i <- seek #initial
            #timer .= d
            #children %= cons (mkFish i)

days :: (Num a, Ord a) => Int -> Sea a [Fish a] ()
days n = replicateM_ n day

simulate :: (Num a, Ord a) => Int -> [a] -> Int
simulate d ns = days d
    & execSea grow (mkFish <$> ns)
    & fmap length
    & sum

part1 :: (Num a, Ord a) => [a] -> Int
part1 = simulate 80

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-06.txt"
    case getFish @Int text of
        Left err -> die err
        Right fish -> do
            print $ part1 fish
