{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}

module Day12 where

import Control.Applicative (some)
import Control.Arrow ((<<<))
import Control.Monad (when)
import Control.Monad.State (State)
import Control.Monad.State qualified as State
import Data.Char qualified as Char
import Data.Containers.ListUtils (nubInt)
import Data.ExtendedReal (Extended)
import Data.ExtendedReal qualified as Ext
import Data.Foldable (for_)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Matrix (Matrix)
import Data.Matrix qualified as Matrix
import Data.PQueue.Prio.Min (MinPQueue)
import Data.PQueue.Prio.Min qualified as Queue
import GHC.Generics (Generic)
import Linear (V2(V2))
import Optics ((%), (&), zoom, use, at)
import Optics.State.Operators ((%=), (?=))
import System.Exit (die)
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.C

import Common (Parser, cons, readInputFileUtf8)


-- types

data AStar = MkAStar
    { openSet :: MinPQueue (Extended Int) (V2 Int)
    , cameFrom :: Map (V2 Int) (V2 Int)
    , scoreFromStart :: Map (V2 Int) (Extended Int)
    , scoreToGoal :: Map (V2 Int) (Extended Int) }
    deriving (Eq, Generic, Ord, Show)

data Dijkstra = MkDijkstra
    { openQueue :: MinPQueue (Extended Int) (V2 Int)
    , previous :: Map (V2 Int) (V2 Int)
    , distToEnd :: Map (V2 Int) (Extended Int) }
    deriving (Eq, Generic, Ord, Show)


-- parsing

parseRow :: Parser [Char]
parseRow = some $ Parse.satisfy $ not <<< Char.isSpace

parseMatrix :: Parser (Matrix Char)
parseMatrix = do
    rows <- parseRow `Parse.sepEndBy` Parse.C.eol
    case nubInt (length <$> rows) of
        [_] -> pure $ Matrix.fromLists rows
        _   -> fail "rows unequal in length"

parseHill :: Parser (V2 Int, V2 Int, Matrix Char)
parseHill = do
    matrix <- parseMatrix
    let pts = V2 <$> [1 .. Matrix.ncols matrix] <*> [1 .. Matrix.nrows matrix]
    let findPt char = do
            i <- List.findIndex (\pt -> matrix .!. pt == char) pts
            pure $ pts !! i
    case (findPt 'S', findPt 'E') of
        (Nothing, _) -> fail "no start 'S' on hill"
        (_, Nothing) -> fail "no end 'E' on hill"
        (Just start, Just end) -> do
            let hill = matrix & setElem 'a' start & setElem 'z' end
            pure (start, end, hill)

-- general-purpose functions

(.!.) :: Matrix a -> V2 Int -> a
matrix .!. V2 x y = matrix Matrix.! (y, x)

setElem :: a -> V2 Int -> Matrix a -> Matrix a
setElem val (V2 x y) = Matrix.setElem val (y, x)

inBounds :: V2 Int -> Matrix a -> Bool
inBounds (V2 x y) matrix =
    (1 <= x && x <= Matrix.ncols matrix) &&
    (1 <= y && y <= Matrix.nrows matrix)

-- | Adds the given key-value pair into the queue. If it is already present
-- somewhere in the queue, then it does nothing.
insertQueue :: (Ord k, Eq a) => k -> a -> MinPQueue k a -> MinPQueue k a
insertQueue key val queue
    | val `elem` Queue.elemsU queue = queue
    | otherwise = Queue.insert key val queue

popQueue :: Ord k => State (MinPQueue k a) (Maybe a)
popQueue = State.state $ \queue -> case Queue.minView queue of
    Nothing         -> (Nothing, queue)
    Just (val, new) -> (Just val, new)

adjacent :: Num a => V2 a -> [V2 a]
adjacent (V2 x y) =
    [ V2 x (y + 1)
    , V2 (x + 1) y
    , V2 x (y - 1)
    , V2 (x - 1) y ]

manhattan :: Num a => V2 a -> V2 a -> a
manhattan u v = sum $ abs $ v - u


-- A* implementation

aStar :: Matrix Char -> V2 Int -> State AStar (Maybe [V2 Int])
aStar matrix goal = zoom #openSet popQueue >>= \case
    Nothing -> pure Nothing
    Just cur | cur == goal -> fmap (Just <<< reverse) (backtrace cur)
    Just cur -> do
        let check nbr = (nbr `inBounds` matrix) && (value nbr <= value cur + 1)
        let neighbours = filter check $ adjacent cur
        for_ neighbours $ \nbr -> do
            tentative <- (+ Ext.Finite 1) <$> fromStart cur
            score <- fromStart nbr
            when (tentative < score) $ do
                let heuristic = tentative + Ext.Finite (manhattan nbr goal)
                #cameFrom % at nbr ?= cur
                #scoreFromStart % at nbr ?= tentative
                #scoreToGoal % at nbr ?= heuristic
                #openSet %= insertQueue heuristic nbr
        aStar matrix goal
  where
    fromStart pt = Map.findWithDefault Ext.PosInf pt <$> use #scoreFromStart
    value pt = Char.ord (matrix .!. pt)

backtrace :: V2 Int -> State AStar [V2 Int]
backtrace current = use (#cameFrom % at current) >>= \case
    Nothing   -> pure [current]
    Just prev -> cons current <$> backtrace prev

shortestPath :: V2 Int -> V2 Int -> Matrix Char -> Maybe [V2 Int]
shortestPath start goal matrix =
    State.evalState (aStar matrix goal) $ MkAStar
        { openSet = Queue.singleton heuristic start
        , cameFrom = Map.empty
        , scoreFromStart = Map.singleton start (Ext.Finite 0)
        , scoreToGoal = Map.singleton start heuristic }
  where
    heuristic = Ext.Finite $ manhattan start goal


dijkstra :: Matrix Char -> State Dijkstra ()
dijkstra matrix = zoom #openQueue popQueue >>= \case
    Nothing -> pure ()
    Just cur -> do
        let check nbr = (nbr `inBounds` matrix) && (value cur <= value nbr + 1)
        let neighbours = filter check $ adjacent cur
        for_ neighbours $ \nbr -> do
            tentative <- (+ Ext.Finite 1) <$> toEnd cur
            score <- toEnd nbr
            when (tentative < score) $ do
                #previous % at nbr ?= cur
                #distToEnd % at nbr ?= tentative
                #openQueue %= insertQueue tentative nbr
        dijkstra matrix
  where
    toEnd pt = Map.findWithDefault Ext.PosInf pt <$> use #distToEnd
    value pt = Char.ord (matrix .!. pt)

traceback :: V2 Int -> State Dijkstra [V2 Int]
traceback current = use (#previous % at current) >>= \case
    Nothing   -> pure [current]
    Just prev -> cons current <$> traceback prev

shortestTrail :: Char -> V2 Int -> Matrix Char -> Maybe [V2 Int]
shortestTrail char end matrix = flip State.evalState initial $ do
    dijkstra matrix
    paths <- Map.filterWithKey fromChar <$> use #distToEnd
    let minDist x = x == Map.foldl' min Ext.PosInf paths
    case Map.keys $ Map.filter minDist paths of
        path : _ -> fmap (Just <<< reverse) (traceback path)
        _        -> pure Nothing
  where
    initial = MkDijkstra
        { openQueue = Queue.singleton (Ext.Finite 0) end
        , previous = Map.empty
        , distToEnd = Map.singleton end (Ext.Finite 0) }
    fromChar pt _ = matrix .!. pt == char


-- main hook

part1 :: V2 Int -> V2 Int -> Matrix Char -> Maybe Int
part1 start end hill = do
    path <- shortestPath start end hill
    pure $ length path - 1

part2 :: Char -> V2 Int -> Matrix Char -> Maybe Int
part2 char end hill = do
    trail <- shortestTrail char end hill
    pure $ length trail - 1

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-12.txt"
    case Parse.runParser (parseHill <* Parse.eof) "day-12" text of
        Left err -> die $ Parse.errorBundlePretty err
        Right (start, end, hill) -> do
            part1 start end hill
                & maybe "no path exists" show
                & putStrLn
            part2 'a' end hill
                & maybe "no trail exists" show
                & putStrLn
