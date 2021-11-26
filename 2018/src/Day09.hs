{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module Day09 where

import Control.Applicative (Alternative, (<|>))
import Control.Lens ((%=), (.=), use, zoom, ix, at)
import Control.Monad (void)
import Control.Monad.State (State, MonadState)
import Control.Monad.State qualified as State
import Control.Monad.Writer (MonadWriter)
import Control.Monad.Writer qualified as Writer
import Data.Generics.Labels ()  -- for #lens orphan instance
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Flow ((.>))
import GHC.Generics (Generic)

import Common


data Input marble player = MkInput
    { players :: player
    , lastMarble :: marble
    } deriving (Eq, Ord, Read, Show)

data Game marble player = MkGame
    { marblesCircle :: Seq marble
    , curMarbleIx :: Int
    , marblesNext :: [marble]
    , curPlayer :: player
    , numPlayers :: player
    , playerScores :: Map player [marble]
    } deriving (Eq, Generic, Ord, Read, Show)

data Turn = Done | Stuck
    deriving (Eq, Ord, Read, Show)

pop :: State [a] (Maybe a)
pop = State.get >>= \case
    []     -> pure Nothing
    x : xs -> Just x <$ State.put xs

popAt :: Int -> State (Seq a) (Maybe a)
popAt i = State.gets (Seq.lookup i) >>= \case
    Nothing -> pure Nothing
    Just x  -> Just x <$ State.modify (Seq.deleteAt i)

divides :: Integral a => a -> a -> Bool
divides m n = n `mod` m == 0

alternate :: Alternative f => a -> f a -> f a
alternate x act = act <|> pure x

consMaybe :: a -> Maybe [a] -> Maybe [a]
consMaybe x = maybe [x] (x :) .> Just

takeTurn
    :: ( Integral marble
       , Ord player
       , MonadState (Game marble player) m
       )
    => m Turn
takeTurn = zoomed #marblesNext pop >>= \case
    Just next | 23 `divides` next -> do
        player <- use #curPlayer
        #playerScores . at player %= consMaybe next
        i <- use #curMarbleIx
        len <- Seq.length <$> use #marblesCircle
        let j = (i - 8) `mod` len
        zoomed #marblesCircle (popAt j) >>= \case
            Just other -> #playerScores . ix player %= (other :)
            Nothing    -> pure ()
        #curMarbleIx .= (j + 1) `mod` len
        pure Done
    Just next -> do
        i <- use #curMarbleIx
        #marblesCircle %= Seq.insertAt (i + 1) next
        len <- Seq.length <$> use #marblesCircle
        #curMarbleIx .= (i + 2) `mod` len
        pure Done
    Nothing -> pure Stuck
  where
    zoomed lens f = State.state $ State.runState $ zoom lens f

playGame
    :: ( Integral marble
       , Integral player
       , MonadState (Game marble player) m
       )
    => m ()
playGame = void $ Writer.runWriterT logGame

logGame
    :: ( Integral marble
       , Integral player
       , MonadState (Game marble player) m
       , MonadWriter [Game marble player] m
       )
    => m ()
logGame = takeTurn >>= \case
    Stuck -> pure ()
    Done  -> do
        State.gets singleton >>= Writer.tell
        n <- use #numPlayers
        #curPlayer %= (+ 1) .> (`mod` n)
        logGame
  where
    singleton x = [x]

makeInitGame
    :: (Integral marble, Integral player)
    => Input marble player -> Game marble player
makeInitGame MkInput{..} = MkGame
    { marblesCircle = Seq.singleton 0
    , curMarbleIx = 0
    , marblesNext = [1 .. lastMarble]
    , curPlayer = 0
    , numPlayers = players
    , playerScores = Map.empty
    }

runGame
    :: (Integral marble, Integral player)
    => Input marble player -> Game marble player
runGame = makeInitGame .> State.execState playGame

debugGame
    :: (Integral marble, Integral player)
    => Input marble player -> [Game marble player]
debugGame = makeInitGame .> State.execStateT logGame .> Writer.execWriter

highScore :: Integral marble => Game marble player -> marble
highScore = playerScores .> fmap sum .> supremum 0

part1 :: (Integral marble, Integral player) => Input marble player -> marble
part1 = runGame .> highScore

main :: IO ()
main = do
    print $ part1 input
  where
    input :: Input Int Int
    input = MkInput
        { players = 463
        , lastMarble = 71787
        }
