{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneKindSignatures #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Common.Matrix
    ( Matrix(MkMatrix, unMatrix)
    , fromVectors

    , Fin(unFin)
    , next, prev

    , xMax, xMin, yMin, yMax
    , xIndices, yIndices, index
    ) where

import Data.Function ((&))
import Data.Kind (Type)
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import GHC.TypeNats (Nat)
import Instances.TH.Lift ()  -- provides @instance Lift Vector@
import Language.Haskell.TH.Syntax (Lift)
import Linear.V (V, Dim)
import Linear.V qualified as V


type Matrix :: Nat -> Nat -> Type -> Type
newtype Matrix x y a = MkMatrix
    { unMatrix :: V y (V x a)
    } deriving (Eq, Ord, Show, Lift)

deriving instance Lift a => Lift (V n a)  -- orphan instance

type Fin :: Nat -> Type
newtype Fin n = UnsafeMkFin
    { unFin :: Int
    } deriving (Eq, Ord, Show)

fromVectors :: (Dim x, Dim y) => Vector (Vector a) -> Maybe (Matrix x y a)
fromVectors outer = do
    inner <- V.fromVector outer
    MkMatrix <$> traverse V.fromVector inner

next :: Dim n => Fin n -> Maybe (Fin n)
next fin@(UnsafeMkFin num)
    | num < top = Just $ UnsafeMkFin $ num + 1
    | otherwise = Nothing
  where
    top = V.reflectDim fin - 1

prev :: Fin n -> Maybe (Fin n)
prev (UnsafeMkFin num)
    | num > 0   = Just $ UnsafeMkFin $ num - 1
    | otherwise = Nothing

index :: Fin x -> Fin y -> Matrix x y a -> a
index xFin yFin (MkMatrix matrix) =
    matrix & get (unFin yFin) & get (unFin xFin)
  where
    get n vec = V.toVector vec Vec.! n

xMin :: Matrix x y a -> Fin x
xMin _ = UnsafeMkFin 0

yMin :: Matrix x y a -> Fin y
yMin _ = UnsafeMkFin 0

xMax :: Dim x => Matrix x y a -> Fin x
xMax (MkMatrix matrix) = UnsafeMkFin $ V.dim (Vec.head $ V.toVector matrix) - 1

yMax :: Dim y => Matrix x y a -> Fin y
yMax (MkMatrix matrix) = UnsafeMkFin $ V.dim matrix - 1

xIndices :: Dim x => Matrix x y a -> [Fin x]
xIndices matrix = UnsafeMkFin <$> [0 .. unFin (xMax matrix)]

yIndices :: Dim y => Matrix x y a -> [Fin y]
yIndices matrix = UnsafeMkFin <$> [0 .. unFin (yMax matrix)]
