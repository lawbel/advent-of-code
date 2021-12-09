{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Day09 where

import Control.Applicative (some)
import Control.Monad (guard)
import Data.Char qualified as Char
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Vector qualified as Vec
import Flow ((.>))
import GHC.TypeNats (Nat)
import Linear.V (V, Dim)
import Linear.V qualified as V
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch

import Common
import Common.Matrix


-- types

data SomeVec a where
    MkSomeVec :: forall (n :: Nat) a. Dim n => V n a -> SomeVec a

deriving instance Show a => Show (SomeVec a)

data SomeMatrix a where
    MkSomeMatrix
        :: forall (x :: Nat) (y :: Nat) a
        .  (Dim x, Dim y)
        => Matrix x y a
        -> SomeMatrix a

deriving instance Show a => Show (SomeMatrix a)

-- parsing

parseDecimalDigit :: Num a => Parser a
parseDecimalDigit = fmap (Char.digitToInt .> fromIntegral) Par.Ch.digitChar

parseVec :: Num a => Parser (SomeVec a)
parseVec = do
    vec <- Vec.fromList <$> some parseDecimalDigit
    pure $ V.reifyVectorNat vec MkSomeVec

parseMatrix :: Num a => Parser (SomeMatrix a)
parseMatrix = do
    MkSomeVec row <- parseVec <* Par.Ch.eol
    let parseRowN = V.V <$> Vec.replicateM (V.dim row) parseDecimalDigit
    rows <- parseRowN `Par.sepEndBy` Par.Ch.eol
    let matrix = Vec.fromList (row : rows)
    pure $ V.reifyVectorNat matrix $ MkMatrix .> MkSomeMatrix

getMatrix :: Num a => Text -> Either String (SomeMatrix a)
getMatrix = runParser "day-09" $ parseMatrix <* Par.Ch.space <* Par.eof

-- main logic

nbrs :: Dim n => Fin n -> [Fin n]
nbrs fin = catMaybes [prev fin, next fin]

{-# ANN lowPoints "HLINT: ignore" #-}
lowPoints :: (Dim x, Dim y, Ord a) => Matrix x y a -> Map (Fin x, Fin y) a
lowPoints matrix = Map.fromList $ do
    base@(xBase, yBase) <- (,) <$> xIndices matrix <*> yIndices matrix
    let val = index xBase yBase matrix
    guard $ all (> val) $ getIndex <$> nbrs xBase <*> pure yBase
    guard $ all (> val) $ getIndex <$> pure xBase <*> nbrs yBase
    pure (base, val)
  where
    getIndex xIx yIx = index xIx yIx matrix

sumOfRiskOfLowPoints :: (Dim x, Dim y, Num a, Ord a) => Matrix x y a -> a
sumOfRiskOfLowPoints = lowPoints .> fmap (+ 1) .> sum

part1 :: (Dim x, Dim y, Num a, Ord a) => Matrix x y a -> a
part1 = sumOfRiskOfLowPoints

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-09.txt"
    case getMatrix @Int text of
        Left err -> die err
        Right (MkSomeMatrix matrix) -> do
            print $ part1 matrix
