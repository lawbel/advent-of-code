{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString qualified as B
import Data.Foldable (minimumBy)
import Data.Ord (comparing)
import Data.Set qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Flow ((.>))


data Direc = L | R | U | D
    deriving (Bounded, Enum, Eq, Ord, Show, Read)

main :: IO ()
main = do
    input <- parseInput <$> readUtf8File "input.txt"
    print $ uncurry part1 input

part1 :: [(Direc, Int)] -> [(Direc, Int)] -> Int
part1 one two = manhattan $ minCrossover one two

minCrossover :: [(Direc, Int)] -> [(Direc, Int)] -> (Int, Int)
minCrossover one two =
    minimumBy (comparing manhattan) $
        S.intersection (pointSet one) (pointSet two)

pointSet :: [(Direc, Int)] -> S.Set (Int, Int)
pointSet = trail .> S.fromList

trail :: [(Direc, Int)] -> [(Int, Int)]
trail list = case list of
    [] -> []
    (dir, n) : xs ->
        mkVecs dir n <> fmap (addPt $ mkVec dir n) (trail xs)

mkVec :: Direc -> Int -> (Int, Int)
mkVec dir n = case dir of
    L -> (-n, 0)
    R -> (n, 0)
    U -> (0, n)
    D -> (0, -n)

mkVecs :: Direc -> Int -> [(Int, Int)]
mkVecs dir n = case dir of
    L -> [ (-x, 0) | x <- [1..n] ]
    R -> [ (x, 0) | x <- [1..n] ]
    U -> [ (0, x) | x <- [1..n] ]
    D -> [ (0, -x) | x <- [1..n] ]

addPt :: (Int, Int) -> (Int, Int) -> (Int, Int)
addPt (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

manhattan :: (Int, Int) -> Int
manhattan (x1, y1) = abs x1 + abs y1

readUtf8File :: FilePath -> IO T.Text
readUtf8File path = TE.decodeUtf8 <$> B.readFile path

parseInput :: T.Text -> ([(Direc, Int)], [(Direc, Int)])
parseInput txt = case T.lines txt of
    [one, two] -> (parseLine one, parseLine two)
  where
    parseLine :: T.Text -> [(Direc, Int)]
    parseLine = T.splitOn "," .> fmap readArc

    readArc :: T.Text -> (Direc, Int)
    readArc txt = (dir, dist)
      where
        dir = read [T.head txt]
        dist = case TR.decimal (T.tail txt) of
            Left _ -> error "parseInput: input not an Int"
            Right (n, _) -> n
