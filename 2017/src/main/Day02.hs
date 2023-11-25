module Day02 where

import Common
import Common.Vector qualified as Comm.Vec
import Control.Monad (guard)
import Data.Maybe (listToMaybe)
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Flow ((.>))
import Data.Text qualified as Text
import Data.Char qualified as Char


maxDiff :: (Num a, Ord a, Bounded a) => Vector a -> a
maxDiff row
    | Vec.null row = 0
    | otherwise =
        Comm.Vec.maximum minBound row - Comm.Vec.minimum maxBound row

checksum :: (Bounded a, Ord a, Num a) => Vector (Vector a) -> a
checksum = fmap maxDiff .> sum

evenDiv :: Integral a => Vector a -> Maybe a
evenDiv vec = listToMaybe $ do
    i <- [0 .. Vec.length vec]
    j <- [0 .. i - 1]
    let x = vec Vec.! i
    let y = vec Vec.! j
    let multiple = lcm x y
    guard $ multiple `elem` [x, y]
    pure $ multiple `div` gcd x y

checksum2 :: Integral a => Vector (Vector a) -> a
checksum2 = fmap (evenDiv .> maybe 0 id) .> sum

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-02.txt"
    let rows = Text.split Char.isSpace <$> Text.lines text
    let raw = textRead @Int <<$>> rows
    let sheet = Vec.fromList <$> Vec.fromList raw
    print $ checksum sheet
    print $ checksum2 sheet
