{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE QuasiQuotes #-}

module Day10 where

import Data.ByteString qualified as B
import Data.List qualified as L
import Data.Ord (comparing)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import Flow ((.>))
import Language.Haskell.Printf qualified as Printf
import Control.Applicative (empty, liftA2)


data Tile = Empty | Asteroid
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data V2 a = MkV2 !a !a
    deriving (Bounded, Eq, Functor, Foldable, Ord, Read, Show)

instance Applicative V2 where
    pure x = MkV2 x x
    MkV2 f1 f2 <*> MkV2 x1 x2 = MkV2 (f1 x1) (f2 x2)

instance Num a => Num (V2 a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = fromInteger .> pure

type Grid a = V.Vector (V.Vector a)

main :: IO ()
main = do
    input <- parseInput <$> readFileUtf8 "day-10/input.txt"
    print $ part1 input

part1 :: Grid Tile -> Int
part1 grid = S.size $ snd $
    maxMatchesRadiatingOut Asteroid (indicesOf Asteroid grid) grid

maxMatchesRadiatingOut
    :: Eq a => a -> S.Set (V2 Int) -> Grid a -> (V2 Int, S.Set (V2 Int))
maxMatchesRadiatingOut x pts grid =
    L.maximumBy (comparing $ snd .> length) $
        allMatchesRadiatingOut x pts grid

allMatchesRadiatingOut
    :: Eq a => a -> S.Set (V2 Int) -> Grid a -> [(V2 Int, S.Set (V2 Int))]
allMatchesRadiatingOut x pts grid = do
    pt <- S.toList pts
    pure (pt, firstMatchesRadiatingOut x pt grid)

firstMatchesRadiatingOut :: Eq a => a -> V2 Int -> Grid a -> S.Set (V2 Int)
firstMatchesRadiatingOut x base grid = S.fromList $ do
    ray <- pointsFromTo base <$> S.toList (boundaryPoints grid)
    case filter (\pt -> index grid pt == x) ray of
        [] -> empty
        pt : _ -> pure pt

boundaryPoints :: Grid a -> S.Set (V2 Int)
boundaryPoints grid = S.fromList $ mconcat
    [ [ MkV2 0 y | y <- [0..rows-1] ]
    , [ MkV2 (cols - 1) y | y <- [0..rows-1] ]
    , [ MkV2 x 0 | x <- [1..cols-2] ]
    , [ MkV2 x (rows - 1) | x <- [1..cols-2] ]
    ]
  where
    rows = V.length grid
    cols = V.length $ V.head grid

pointsFromTo :: V2 Int -> V2 Int -> [V2 Int]
pointsFromTo p1@(MkV2 x1 y1) p2@(MkV2 x2 y2) = do
    i <- [1 .. count]
    pure $ p1 + pure i * step
  where
    xDiff = abs $ x2 - x1
    yDiff = abs $ y2 - y1
    count = gcd xDiff yDiff
    step = fmap (`div` count) (p2 - p1)

index :: Grid a -> V2 Int -> a
index grid (MkV2 x y) = grid V.! y V.! x

indicesOf :: Eq a => a -> Grid a -> S.Set (V2 Int)
indicesOf val grid = S.fromList $ V.toList $ do
    (y, vec) <- V.indexed grid
    x <- V.elemIndices val vec
    pure $ MkV2 x y

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path

parseInput :: T.Text -> Grid Tile
parseInput =
    T.lines
        .> V.fromList
        .> fmap (T.unpack .> V.fromList .> fmap charToTile)
  where
    charToTile c = case c of
        '.' -> Empty
        '#' -> Asteroid
        _ -> error $ [Printf.s|parseInput: char '%c' not in {'.', '#'}|] c
