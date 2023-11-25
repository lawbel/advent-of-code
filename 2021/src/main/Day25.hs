{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}

module Day25 where

import Control.Applicative (some)
import Data.Foldable (asum)
import Data.Functor (($>))
import Data.List.NonEmpty qualified as List.NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Flow ((.>))
import Language.Haskell.TH.Syntax (Lift)
import Linear.V2 (V2(V2))
import Optics (view)
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch

import Common
import Common.Optics


-- types

data Cucumber = East | South
    deriving (Eq, Lift, Ord, Read, Show)

-- parsing

parseLocation :: Parser (Maybe Cucumber)
parseLocation = asum
    [ Par.Ch.char '>' $> Just East
    , Par.Ch.char 'v' $> Just South
    , Par.Ch.char '.' $> Nothing ]

parseRow :: Integral a => Parser [(a, Maybe Cucumber)]
parseRow = zip [0..] <$> some parseLocation

parseRows :: Integral a => Parser [(V2 a, Maybe Cucumber)]
parseRows = do
    rows <- zip [0..] <$> Par.sepEndBy parseRow Par.Ch.space
    pure $ do
        (y, row) <- rows
        (x, cucumber) <- row
        pure (V2 x y, cucumber)

parseSeaFloor :: Integral a => Parser (V2 a, Map (V2 a) Cucumber)
parseSeaFloor = do
    sea <- parseRows
    let bounds = do
            let vecs = fmap fst sea
            xs <- List.NE.nonEmpty $ fmap (view _x) vecs
            ys <- List.NE.nonEmpty $ fmap (view _y) vecs
            pure $ V2 (supremum1 xs + 1) (supremum1 ys + 1)
    case bounds of
        Nothing  -> fail "empty map"
        Just vec -> pure (vec, Map.fromList $ mapMaybe sequence sea)

getSeaFloor :: Integral a => Text -> Either String (V2 a, Map (V2 a) Cucumber)
getSeaFloor = runParser "day-25" $ parseSeaFloor <* Par.eof

-- pretty printing

prettyCucumber :: Cucumber -> Char
prettyCucumber = \case
    East  -> '>'
    South -> 'v'

prettySeaFloor :: (Enum a, Num a, Ord a) => V2 a -> Map (V2 a) Cucumber -> Text
prettySeaFloor size = prettyGrid (pure 0) (subtract 1 <$> size) $ \case
    Nothing       -> Text.singleton '.'
    Just cucumber -> Text.singleton $ prettyCucumber cucumber

-- core logic

moveEast :: Integral a => a -> Map (V2 a) Cucumber -> Map (V2 a) Cucumber
moveEast bound grid = flip Map.mapKeys grid $ \vec@(V2 x y) ->
    let next = V2 (mod (x + 1) bound) y
        canMove = (grid Map.! vec == East) && (next `Map.notMember` grid)
    in  if canMove then next else vec

moveSouth :: Integral a => a -> Map (V2 a) Cucumber -> Map (V2 a) Cucumber
moveSouth bound grid = flip Map.mapKeys grid $ \vec@(V2 x y) ->
    let next = V2 x (mod (y + 1) bound)
        canMove = (grid Map.! vec == South) && (next `Map.notMember` grid)
    in  if canMove then next else vec

move :: Integral a => V2 a -> Map (V2 a) Cucumber -> Map (V2 a) Cucumber
move (V2 x y) = moveEast x .> moveSouth y

moves
    :: Integral a
    => Int -> V2 a -> Map (V2 a) Cucumber -> Map (V2 a) Cucumber
moves n size = compose n (move size)

moveFixpoint
    :: Integral a
    => V2 a -> Map (V2 a) Cucumber -> (Int, Map (V2 a) Cucumber)
moveFixpoint size = composeFixpoint (move size)

composeFixpoint :: Eq a => (a -> a) -> a -> (Int, a)
composeFixpoint f = go 0
  where
    go !n prev =
        let next = f prev
        in  if next == prev then (n, prev) else go (n + 1) next

stopsAfter :: Integral a => V2 a -> Map (V2 a) Cucumber -> Int
stopsAfter size = moveFixpoint size .> fst .> (+ 1)

-- main

part1 :: Integral a => V2 a -> Map (V2 a) Cucumber -> Int
part1 = stopsAfter

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-25.txt"
    case getSeaFloor @Int text of
        Left err -> die err
        Right (size, sea) -> do
            print $ part1 size sea
