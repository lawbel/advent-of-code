{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Day21 where

import Control.Monad.State (State, replicateM, gets, when, runState)
import Flow ((.>))
import Optics (Lens', (%), (^.), zoom, equality, use, view)
import Optics.State.Operators ((<%=), (%=))
import Optics.TH (makeFieldLabelsWith, noPrefixFieldLabels)

import Common


-- types

newtype D100 = MkD100
    { unD100 :: Int }
    deriving (Eq, Ord, Read, Show)

makeFieldLabelsWith noPrefixFieldLabels ''D100

data Player = One | Two
    deriving (Eq, Ord, Read, Show)

data Players a = MkPlayers
    { one :: a
    , two :: a }
    deriving (Eq, Ord, Read, Show)

makeFieldLabelsWith noPrefixFieldLabels ''Players

data Game = MkGame
    { scores :: Players Int
    , positions :: Players Int
    , current :: Player
    , d100 :: D100
    , rolls :: Int }
    deriving (Eq, Ord, Read, Show)

makeFieldLabelsWith noPrefixFieldLabels ''Game

-- auxilliary functions

next :: Player -> Player
next = \case
    One -> Two
    Two -> One

player :: Player -> Lens' (Players a) a
player = \case
    One -> #one
    Two -> #two

modulo :: Integral a => a -> a -> a
n `modulo` modulus = mod (n - 1) modulus + 1

-- game logic

roll :: State Game Int
roll = do
    #rolls += 1
    zoom (#d100 % #unD100) $ do
        equality += 1
        equality <%= (`modulo` 100)

move :: Int -> State Int Int
move n = do
    equality += n
    equality <%= (`modulo` 10)

turn :: State Game ()
turn = do
    play <- player <$> use #current
    moves <- sum <$> replicateM 3 roll
    space <- zoom (#positions % play) $ move moves
    #scores % play += space

turnsWhile :: (Game -> Bool) -> State Game ()
turnsWhile cond = do
    bool <- gets cond
    when bool $ do
        turn
        #current %= next
        turnsWhile cond

playUntil :: Int -> State Game Player
playUntil score = do
    turnsWhile $ \game -> let MkPlayers n m = scores game in max n m < score
    next <$> use #current

makeGame :: Players Int -> Game
makeGame pos = MkGame
    { scores = MkPlayers 0 0
    , positions = pos
    , current = One
    , d100 = MkD100 0
    , rolls = 0 }

-- parts & main hook

part1 :: Players Int -> Int
part1 = makeGame .> runState (playUntil 1000) .> uncurry score
  where
    score :: Player -> Game -> Int
    score winner game =
        let loss = game ^. #scores % player (next winner)
            count = game ^. #rolls
        in  loss * count

main :: IO ()
main = do
    print $ part1 pos
  where
    pos = MkPlayers
        { one = 4
        , two = 3 }
