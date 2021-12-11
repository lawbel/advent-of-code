{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Day11 where

import Control.Monad (replicateM, guard)
import Control.Monad.State (State, get, execState)
import Data.Char qualified as Char
import Data.Data (Proxy(Proxy))
import Data.Foldable (for_, traverse_)
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as Vec
import Flow ((.>))
import GHC.TypeNats (Nat)
import Linear.V (V, Dim)
import Linear.V qualified as V
import Optics ((%), (&), use, ix, view, mapped, _2)
import Optics.State.Operators ((%=), (.=))
import Optics.TH (noPrefixFieldLabels, makeFieldLabelsWith)
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch

import Common
import Common.Matrix
import Control.Arrow ((&&&))


-- types

data Step x y a = MkStep
    { flashed :: Set (Fin x, Fin y)
    , matrix :: Matrix x y a
    , count :: Int
    } deriving (Eq, Ord, Show)

makeFieldLabelsWith noPrefixFieldLabels ''Step

-- parsing

parseDecimalDigit :: Num a => Parser a
parseDecimalDigit = fmap (Char.digitToInt .> fromIntegral) Par.Ch.digitChar

parseRow :: forall (n :: Nat) a. Dim n => Num a => Parser (V n a)
parseRow = do
    digits <- replicateM dim parseDecimalDigit
    case V.fromVector $ Vec.fromList digits of
        Just vec -> pure vec
        Nothing  -> fail "bad row length"
  where
    dim = V.reflectDim $ Proxy @n

parseGrid :: forall x y a. (Dim x, Dim y, Num a) => Parser (Matrix x y a)
parseGrid = do
    digits <- replicateM yDim (parseRow <* Par.Ch.eol)
    case V.fromVector $ Vec.fromList digits of
        Just vec -> pure $ MkMatrix vec
        Nothing  -> fail "bad grid length"
  where
    yDim = V.reflectDim $ Proxy @y

getGrid :: (Dim x, Dim y, Num a) => Text -> Either String (Matrix x y a)
getGrid = runParser "day-11" $ parseGrid <* Par.Ch.space <* Par.eof

-- logic

adjacent :: (Dim x, Dim y) => Fin x -> Fin y -> [(Fin x, Fin y)]
adjacent xFin yFin = do
    xAdj <- catMaybes [prev xFin, pure xFin, next xFin]
    yAdj <- catMaybes [prev yFin, pure yFin, next yFin]
    guard $ (xAdj /= xFin) || (yAdj /= yFin)
    pure (xAdj, yAdj)

flash
    :: (Dim x, Dim y, Num a, Ord a)
    => a -> Matrix x y a -> (Matrix x y a, Int)
flash level grid = (view #matrix &&& view #count) $
    flip execState (MkStep Set.empty grid 0) do
        #matrix % mapped += 1
        flashLoops level
        use #flashed >>= traverse_ \pt -> do
            #matrix % ix pt .= 0
            #count += 1

flashLoops :: (Dim x, Dim y, Num a, Ord a) => a -> State (Step x y a) ()
flashLoops level = do
    before <- get
    flashLoop level
    after <- get
    if before == after then pure () else flashLoops level

flashLoop :: (Dim x, Dim y, Num a, Ord a) => a -> State (Step x y a) ()
flashLoop level = do
    grid <- use #matrix
    let pts = (,) <$> xIndices grid <*> yIndices grid
    for_ pts \pt -> do
        seen <- Set.member pt <$> use #flashed
        val <- use $ #matrix % ix pt
        if not seen && (val > level)
            then do
                #flashed %= Set.insert pt
                for_ (uncurry adjacent pt) \nbr -> #matrix % ix nbr += 1
            else pure ()

iter :: Int -> (a -> a) -> a -> a
iter n f
    | n <= 0    = id
    | otherwise = f .> iter (n - 1) f

flashes
    :: (Dim x, Dim y, Num a, Ord a)
    => Int -> Matrix x y a -> a -> (Matrix x y a, Int)
flashes n grid level = iter n step (grid, 0)
  where
    step (mat, num) = flash level mat & _2 +~ num

part1 :: (Dim x, Dim y, Num a, Ord a) => Matrix x y a -> Int
part1 grid = snd $ flashes 100 grid 9

-- main

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-11.txt"
    case getGrid @10 @10 @Int text of
        Left err -> die err
        Right grid -> do
            print $ part1 grid
