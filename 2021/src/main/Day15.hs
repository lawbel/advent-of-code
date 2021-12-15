{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Day15 where

import Control.Applicative (some)
import Control.Monad.State (State, evalState, when)
import Data.Char qualified as Char
import Data.Foldable (for_)
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HMap
import Data.Kind (Type)
import Data.Maybe (catMaybes)
import Data.PQueue.Prio.Min (MinPQueue)
import Data.PQueue.Prio.Min qualified as Queue
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Vector qualified as Vec
import Flow ((.>))
import GHC.TypeLits (KnownNat)
import GHC.TypeNats (Nat, type (*))
import Linear.V (V, Dim)
import Linear.V qualified as V
import Optics ((&), (%), (^?), (^.), ix, at, use)
import Optics.State.Operators ((%=), (?=), (.=))
import Optics.TH (makeFieldLabelsWith, noPrefixFieldLabels, makePrisms)
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch
import Unsafe.Coerce (unsafeCoerce)

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
    { open :: MinPQueue (Distance a) (Fin x, Fin y)
    , cheap :: HashMap (Fin x, Fin y) (Distance a)
    , guess :: HashMap (Fin x, Fin y) (Distance a)
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

-- combining matrices

quintuple
    :: Num a => (a -> a -> a) -> Matrix x y a -> Matrix (5 * x) (5 * y) a
quintuple f = quintupleX f .> quintupleY f

quintupleX :: Num a => (a -> a -> a) -> Matrix x y a -> Matrix (5 * x) y a
quintupleX f mat =
    appendX (f 0 <$> mat) $
    appendX (f 1 <$> mat) $
    appendX (f 2 <$> mat) $
    appendX (f 3 <$> mat) (f 4 <$> mat)

quintupleY :: Num a => (a -> a -> a) -> Matrix x y a -> Matrix x (5 * y) a
quintupleY f mat =
    appendY (f 0 <$> mat) $
    appendY (f 1 <$> mat) $
    appendY (f 2 <$> mat) $
    appendY (f 3 <$> mat) (f 4 <$> mat)

nextMod9 :: Integral a => a -> a -> a
nextMod9 n val = 1 + mod (val + n - 1) 9

-- logic

add :: Num a => Distance a -> Distance a -> Distance a
add dist1 dist2 = case (dist1, dist2) of
    (Finite x, Finite y) -> Finite $ x + y
    _                    -> Infinite

aStar
    :: (Dim x, Dim y, Num a, Ord a)
    => (Fin x, Fin y) -> (Fin x, Fin y) -> Matrix x y a -> Distance a
aStar from to matrix = evalState (runAStar matrix to) $ MkAStar
    { open = Queue.singleton (Finite 0) from
    , cheap = HMap.singleton from $ Finite 0
    , guess = HMap.singleton from $ manhattan to from
    }

runAStar
    :: (Dim x, Dim y, Num a, Ord a)
    => Matrix x y a -> (Fin x, Fin y) -> State (AStar x y a) (Distance a)
runAStar matrix to = getNext >>= \case
    Nothing -> pure Infinite
    Just (cur, dist) | cur == to -> pure dist
    Just (cur, _) -> do
        for_ (uncurry neighbours cur) $ \nbr -> do
            new <- tentative matrix cur nbr
            best <- HMap.findWithDefault Infinite nbr <$> use #cheap
            when (new < best) $ do
                #cheap % at nbr ?= new
                let guessed = add new (manhattan to nbr)
                #guess % at nbr ?= guessed
                #open %= Queue.insert guessed nbr
        runAStar matrix to

{-# ANN getNext "HLINT: ignore" #-}
getNext :: Ord a => State (AStar x y a) (Maybe ((Fin x, Fin y), Distance a))
getNext = fmap Queue.minViewWithKey (use #open) >>= \case
    Nothing -> pure Nothing
    Just ((dist, point), updated) -> do
        #open .= updated
        pure $ Just (point, dist)

tentative
    :: Num a
    => Matrix x y a
    -> (Fin x, Fin y)
    -> (Fin x, Fin y)
    -> State (AStar x y a) (Distance a)
tentative matrix cur nbr = do
    dist <- HMap.findWithDefault Infinite cur <$> use #cheap
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
part1 matrix = aStar from to matrix ^? _Finite
  where
    from = (xMin matrix, yMin matrix)
    to = (xMax matrix, yMax matrix)

-- as @ghc-typelits-knownnat@ doesn't know about 'Dim', we do the conversion
-- between 'Dim' and 'KnownNat' ourselves
part2 :: forall x y a. (Dim x, Dim y, Integral a) => Matrix x y a -> Maybe a
part2 matrix =
    V.reifyDimNat (V.reflectDim $ Proxy @x) $ \(_ :: Proxy x') ->
    V.reifyDimNat (V.reflectDim $ Proxy @y) $ \(_ :: Proxy y') ->
        part2' (unsafeCoerce matrix :: Matrix x' y' a)

-- here @ghc-typelits-knownnat@ is solving @KnownNat n => KnownNat (n * 5)@
-- for @n = x, y@ which is exactly what we need to use 'quintuple'
part2' :: (KnownNat x, KnownNat y, Integral a) => Matrix x y a -> Maybe a
part2' = quintuple nextMod9 .> part1

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-15.txt"
    case getMatrix @Int text of
        Left err -> die err
        Right (MkSomeMatrix matrix) -> do
            part1 matrix & maybe "infinity" show & putStrLn
            part2 matrix & maybe "infinity" show & putStrLn
