{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Day15 where

import Control.Applicative (some)
import Control.Monad.State (State, evalState, when)
import Data.Char qualified as Char
import Data.Foldable (for_)
import Data.Kind (Type)
import Data.List.NonEmpty qualified as List.NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Vector qualified as Vec
import Flow ((.>))
import GHC.TypeNats (Nat)
import Linear.V (V, Dim)
import Linear.V qualified as V
import Optics ((&), (%), (^?), (^.), ix, at, use)
import Optics.State.Operators ((%=), (?=))
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

data AStar x y a = MkAStar
    { open :: Set (Fin x, Fin y)
    , cheap :: Map (Fin x, Fin y) (Distance a)
    , guess :: Map (Fin x, Fin y) (Distance a)
    } deriving (Eq, Ord, Show)

makeFieldLabelsWith noPrefixFieldLabels ''AStar

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

aStar
    :: (Dim x, Dim y, Num a, Ord a)
    => (Fin x, Fin y) -> (Fin x, Fin y) -> Matrix x y a -> Distance a
aStar from to matrix = evalState (runAStar matrix to) $ MkAStar
    { open = Set.singleton from
    , cheap = Map.singleton from $ Finite 0
    , guess = Map.singleton from $ manhattan to from
    }

runAStar
    :: (Dim x, Dim y, Num a, Ord a)
    => Matrix x y a -> (Fin x, Fin y) -> State (AStar x y a) (Distance a)
runAStar matrix to = getNext >>= \case
    Nothing -> pure Infinite
    Just cur | cur == to -> Map.findWithDefault Infinite cur <$> use #guess
    Just cur -> do
        #open %= Set.delete cur
        for_ (uncurry neighbours cur) $ \nbr -> do
            new <- tentative matrix cur nbr
            best <- Map.findWithDefault Infinite nbr <$> use #cheap
            when (new < best) $ do
                #cheap % at nbr ?= new
                #guess % at nbr ?= add new (manhattan to nbr)
                #open %= Set.insert nbr
        runAStar matrix to

getNext :: Ord a => State (AStar x y a) (Maybe (Fin x, Fin y))
getNext = do
    dist <- flip (Map.findWithDefault Infinite) <$> use #guess
    opens <- use #open
    let dists = List.NE.nonEmpty $ Map.toList $ Map.fromSet dist opens
    pure $ fmap (infimumBy1 (comparing snd) .> fst) dists

tentative
    :: Num a
    => Matrix x y a
    -> (Fin x, Fin y)
    -> (Fin x, Fin y)
    -> State (AStar x y a) (Distance a)
tentative matrix cur nbr = do
    dist <- Map.findWithDefault Infinite cur <$> use #cheap
    pure $ add dist $ Finite $ matrix ^. ix nbr

{-# ANN neighbours "HLINT: ignore" #-}
neighbours :: (Dim x, Dim y) => Fin x -> Fin y -> [(Fin x, Fin y)]
neighbours x y = concat
    [ (,) <$> nbrs x <*> pure y
    , (,) <$> pure x <*> nbrs y
    ]
  where
    nbrs :: Dim n => Fin n -> [Fin n]
    nbrs fin = catMaybes [prev fin, next fin]

manhattan :: Num a => (Fin x, Fin y) -> (Fin x, Fin y) -> Distance a
manhattan (xFrom, yFrom) (xTo, yTo) = Finite $ fromIntegral $ sum
    [ abs $ unFin xFrom - unFin xTo
    , abs $ unFin yFrom - unFin yTo
    ]

-- main

part1 :: (Dim x, Dim y, Num a, Ord a) => Matrix x y a -> Maybe a
part1 matrix = aStar from to  matrix ^? _Finite
  where
    from = (xMin matrix, yMin matrix)
    to = (xMax matrix, yMax matrix)

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-15.txt"
    case getMatrix @Int text of
        Left err -> die err
        Right (MkSomeMatrix matrix) -> do
            part1 matrix & maybe "???" show & putStrLn
