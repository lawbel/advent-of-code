module Day02 where

import Common
import Common.Vector qualified as Comm.Vec
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Flow ((.>))
import Data.Text qualified as Text
import System.OsPath qualified as Sys
import Data.Char qualified as Char


maxDiff :: (Num a, Ord a, Bounded a) => Vector a -> a
maxDiff row
    | Vec.null row = 0
    | otherwise =
        Comm.Vec.maximum minBound row - Comm.Vec.minimum maxBound row

checksum :: (Bounded a, Ord a, Num a) => Vector (Vector a) -> a
checksum = fmap maxDiff .> sum

main :: IO ()
main = do
    text <- readInputFileUtf8 [Sys.osp|input/day-02.txt|]
    let rows = Text.split Char.isSpace <$> Text.lines text
    let raw = textRead @Int <<$>> rows
    let sheet = Vec.fromList <$> Vec.fromList raw
    print $ checksum sheet
