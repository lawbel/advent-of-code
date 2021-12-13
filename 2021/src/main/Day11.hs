{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Day11 where

import Control.Monad (replicateM, guard, unless, when)
import Control.Monad.State (State, get, execState)
import Data.Char qualified as Char
import Data.Foldable (for_, traverse_)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as Vec
import Flow ((.>))
import Linear.V (V, Dim)
import Linear.V qualified as V
import Optics ((%), (&), use, ix, mapped)
import Optics.State.Operators ((%=), (.=))
import Optics.TH (noPrefixFieldLabels, makeFieldLabelsWith)
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch

import Common
import Common.Matrix


-- types

data Step x y a = MkStep
    { flashed :: Set (Fin x, Fin y)
    , matrix :: Matrix x y a
    , count :: Int
    , step :: Int
    } deriving (Eq, Ord, Show)

makeFieldLabelsWith noPrefixFieldLabels ''Step

-- parsing

parseDecimalDigit :: Num a => Parser a
parseDecimalDigit = fmap (Char.digitToInt .> fromIntegral) Par.Ch.digitChar

parseRow :: Num a => Parser (V 10 a)
parseRow = do
    digits <- replicateM 10 parseDecimalDigit
    case V.fromVector $ Vec.fromList digits of
        Just vec -> pure vec
        Nothing  -> fail "bad row length"

parseGrid :: Num a => Parser (Matrix 10 10 a)
parseGrid = do
    digits <- replicateM 10 (parseRow <* Par.Ch.eol)
    case V.fromVector $ Vec.fromList digits of
        Just vec -> pure $ MkMatrix vec
        Nothing  -> fail "bad grid length"

getGrid :: Num a => Text -> Either String (Matrix 10 10 a)
getGrid = runParser "day-11" $ parseGrid <* Par.Ch.space <* Par.eof

-- logic

adjacent :: (Dim x, Dim y) => Fin x -> Fin y -> [(Fin x, Fin y)]
adjacent xFin yFin = do
    xAdj <- catMaybes [prev xFin, pure xFin, next xFin]
    yAdj <- catMaybes [prev yFin, pure yFin, next yFin]
    guard $ (xAdj /= xFin) || (yAdj /= yFin)
    pure (xAdj, yAdj)

flash :: (Dim x, Dim y, Num a, Ord a) => a -> Matrix x y a -> Step x y a
flash level grid =
    flip execState (MkStep Set.empty grid 0 0) do
        #matrix % mapped += 1
        flashLoops level
        use #flashed >>= traverse_ \pt -> do
            #matrix % ix pt .= 0
            #count += 1
        #step += 1

flashLoops :: (Dim x, Dim y, Num a, Ord a) => a -> State (Step x y a) ()
flashLoops level = do
    before <- get
    flashLoop level
    after <- get
    unless (before == after) $ flashLoops level

flashLoop :: (Dim x, Dim y, Num a, Ord a) => a -> State (Step x y a) ()
flashLoop level = do
    grid <- use #matrix
    let pts = (,) <$> xIndices grid <*> yIndices grid
    for_ pts \pt -> do
        seen <- Set.member pt <$> use #flashed
        val <- use $ #matrix % ix pt
        when (not seen && val > level) do
            #flashed %= Set.insert pt
            for_ (uncurry adjacent pt) \nbr -> #matrix % ix nbr += 1

iter :: Int -> (a -> a) -> a -> a
iter n f
    | n <= 0    = id
    | otherwise = f .> iter (n - 1) f

iterUntil :: (a -> Bool) -> (a -> a) -> a -> a
iterUntil cond f x
    | cond x    = x
    | otherwise = iterUntil cond f (f x)

flashes
    :: (Dim x, Dim y, Num a, Ord a)
    => Int -> Matrix x y a -> a -> Step x y a
flashes n grid level = iter n go $ MkStep Set.empty grid 0 0
  where
    go before = flash level (matrix before)
        & #step +~ step before
        & #count +~ count before

flashUntil
    :: (Dim x, Dim y, Num a, Ord a)
    => (Matrix x y a -> Bool) -> Matrix x y a -> a -> Step x y a
flashUntil cond grid level =
    iterUntil (matrix .> cond) go $ MkStep Set.empty grid 0 0
  where
    go before = flash level (matrix before)
        & #step +~ step before
        & #count +~ count before

part1 :: (Dim x, Dim y, Num a, Ord a) => Matrix x y a -> Int
part1 grid = count $ flashes 100 grid 9

part2 :: (Dim x, Dim y, Num a, Ord a) => Matrix x y a -> Int
part2 grid = step $ flashUntil (all (== 0)) grid 9

-- main

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-11.txt"
    case getGrid @Int text of
        Left err -> die err
        Right grid -> do
            print $ part1 grid
            print $ part2 grid
