{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Day15 where

import Control.Applicative (some)
import Control.Monad.State (State, execState, when)
import Data.Char qualified as Char
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Traversable (for)
import Data.Vector qualified as Vec
import Flow ((.>))
import GHC.TypeNats (Nat)
import Linear.V (V, Dim)
import Linear.V qualified as V
import Optics ((&), (%), (.~), (^.), (^?), ix, at, use, view, _1, _2)
import Optics.State.Operators ((%=), (.=))
import Optics.TH (makeFieldLabelsWith, noPrefixFieldLabels, makePrisms)
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch

import Common
import Common.Matrix


-- types

data SomeVec a where
    MkSomeVec
        :: forall (n :: Nat) (a :: Type)
        .  Dim n
        => V n a
        -> SomeVec a

deriving instance Show a => Show (SomeVec a)

data SomeMatrix a where
    MkSomeMatrix
        :: forall (x :: Nat) (y :: Nat) (a :: Type)
        .  (Dim x, Dim y)
        => Matrix x y a
        -> SomeMatrix a

deriving instance Show a => Show (SomeMatrix a)

data Distance a
    = Finite a
    | Infinite
    deriving (Eq, Ord, Read, Show)

makePrisms ''Distance

data Dijkstra x y a = MkDijkstra
    { nodes :: Matrix x y (a, Distance a)
    , unvisited :: Set (Fin x, Fin y)
    , current :: (Fin x, Fin y)
    } deriving (Eq, Ord, Show)

makeFieldLabelsWith noPrefixFieldLabels ''Dijkstra

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
getMatrix = runParser "day-15" $ parseMatrix <* Par.Ch.space <* Par.eof

-- logic

add :: Num a => Distance a -> Distance a -> Distance a
add dist1 dist2 = case (dist1, dist2) of
    (Finite x, Finite y) -> Finite $ x + y
    _                    -> Infinite

dijkstra
    :: (Dim x, Dim y, Num a, Ord a)
    => Fin x -> Fin y -> Matrix x y a -> Matrix x y (a, Distance a)
dijkstra x y matrix = view #nodes $ execState runDijkstra $ MkDijkstra
    { nodes = matrix
        & fmap (, Infinite)
        & ix (x, y) % _2 .~ Finite 0
    , unvisited = Set.fromList $
        (,) <$> xIndices matrix <*> yIndices matrix
    , current = (x, y)
    }

runDijkstra
    :: forall x y a
    .  (Dim x, Dim y, Num a, Ord a)
    => State (Dijkstra x y a) ()
runDijkstra = do
    cur <- use #current
    unvis <- flip Set.member <$> use #unvisited
    let nbrs = uncurry neighbours cur
    for_ (filter unvis nbrs) $ \nbr -> do
        dist <- distThrough cur nbr
        #nodes % ix nbr % _2 %= min dist
    #unvisited % at cur .= Nothing
    remaining <- Set.toList <$> use #unvisited
    let getMin = infimumBy (comparing fst) (Infinite, cur)
    (dist, new) <- fmap getMin $ for remaining $ \r -> do
        matrix <- use #nodes
        pure (matrix ^. ix r % _2, r)
    when (dist < Infinite) $ do
        #current .= new
        runDijkstra

distThrough
    :: Num a
    => (Fin x, Fin y)
    -> (Fin x, Fin y)
    -> State (Dijkstra x y a) (Distance a)
distThrough from to = do
    matrix <- use #nodes
    let distFrom = matrix ^. ix from % _2
    let distTo = Finite $ matrix ^. ix to % _1
    pure $ add distFrom distTo

{-# ANN neighbours "HLINT: ignore" #-}
neighbours :: (Dim x, Dim y) => Fin x -> Fin y -> [(Fin x, Fin y)]
neighbours x y = concat
    [ (,) <$> nbrs x <*> pure y
    , (,) <$> pure x <*> nbrs y
    ]
  where
    nbrs :: Dim n => Fin n -> [Fin n]
    nbrs fin = catMaybes [prev fin, next fin]

part1 :: (Dim x, Dim y, Num a, Ord a) => Matrix x y a -> Maybe a
part1 matrix =
    dijkstra (xMin matrix) (yMin matrix) matrix
        ^? ix (xMax matrix, yMax matrix) % _2 % _Finite

-- main

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-15.txt"
    case getMatrix @Int text of
        Left err -> die err
        Right (MkSomeMatrix matrix) -> do
            part1 matrix & maybe "???" show & putStrLn
