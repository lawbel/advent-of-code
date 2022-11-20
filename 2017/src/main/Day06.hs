module Day06 where

import Control.Monad.ST.Strict (ST)
import Control.Monad.ST.Strict qualified as ST
import Data.Foldable (for_)
import Data.HashTable.ST.Basic qualified as Table
import Data.STRef.Strict qualified as STRef
import Data.Vector.Instances ()  -- for @Hashable Vector@
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vec
import Data.Vector.Unboxed.Mutable (MVector)
import Data.Vector.Unboxed.Mutable qualified as Vec.M


mostBlocks :: MVector s Int -> ST s (Int, Int)
mostBlocks = Vec.M.ifoldl' updateMax (0, 0)
  where
    updateMax acc@(_, x) j y = if y > x then (j, y) else acc

redistribute :: MVector s Int -> ST s ()
redistribute memory = do
    (i, blocks) <- mostBlocks memory
    Vec.M.write memory i 0
    let len = Vec.M.length memory
    let (common, remainder) = blocks `divMod` len
    for_ [0 .. len - 1] $
        Vec.M.modify memory (+ common)
    for_ [i + 1 .. i + remainder] $ \j ->
        Vec.M.modify memory (+ 1) (j `mod` len)

redistributeUntilRepeat :: MVector s Int -> ST s Int
redistributeUntilRepeat memory = do
    count <- STRef.newSTRef 0
    seen <- Table.new

    let loop = do
            redistribute memory
            STRef.modifySTRef' count (+ 1)
            frozen <- Vec.freeze memory
            Table.lookup seen frozen >>= \case
                Nothing -> Table.insert seen frozen () >> loop
                Just _  -> STRef.readSTRef count

    Vec.freeze memory >>= flip (Table.insert seen) ()
    loop

part1 :: Vector Int -> Int
part1 vector = ST.runST $ Vec.thaw vector >>= redistributeUntilRepeat

main :: IO ()
main = do
    let memory = Vec.fromList
            [4, 10, 4, 1, 8, 4, 9, 14, 5, 1, 14, 15, 0, 15, 3, 5]
    print $ part1 memory
