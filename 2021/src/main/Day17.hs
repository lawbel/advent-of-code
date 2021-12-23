{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Day17 where

import Control.Monad (guard)
import Data.Set qualified as Set
import Flow ((.>))
import Linear (V2)
import Optics ((&), (%), (%~))
import Optics.TH (noPrefixFieldLabels, makeFieldLabelsWith)

import Common


data Probe a = MkProbe
    { position :: V2 a
    , velocity :: V2 a }
    deriving (Eq, Ord, Read, Show)

makeFieldLabelsWith noPrefixFieldLabels ''Probe

shoot :: Num a => V2 a -> Probe a
shoot vel = MkProbe
    { position = pure 0
    , velocity = vel }

drag :: (Num a, Ord a) => a -> a
drag n = case n `compare` 0 of
    LT -> n + 1
    EQ -> n
    GT -> n - 1

step :: (Num a, Ord a) => Probe a -> Probe a
step probe = probe
    & #position +~ velocity probe
    & #velocity % _x %~ drag
    & #velocity % _y %~ subtract 1

trajectory :: (Num a, Ord a) => Probe a -> [V2 a]
trajectory = iterate step .> fmap position

-- any valid x vel. must be one of these
possibleXVelocities :: Integral a => a -> a -> [a]
possibleXVelocities xMin xMax = do
    x <- [0 .. xMax]
    guard $ not $ Set.null $
        Set.fromList [xMin .. xMax] `Set.intersection`
        Set.fromList (partialSumsDecr x)
    pure x

partialSumsDecr :: (Enum a, Num a) => a -> [a]
partialSumsDecr n = reverse $ scanr (+) n [1 .. n-1]

main :: IO ()
main = do
    _text <- readInputFileUtf8 "input/day-17.txt"
    pure ()
