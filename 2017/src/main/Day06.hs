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

-- | Calls 'redistribute' repeatedly, keeping track of the different states of
-- the given vector, until the contents of the vector is repeated.
-- Returns @(cycles, loopSize)@ where:
--
--   * @cycles@ is the total number of times we called 'redistribute'
--   * @loopSize@ is the number of times we called 'redistribute' since we saw
--     the previous occurence of the vector.
redistributeUntilRepeat :: MVector s Int -> ST s (Int, Int)
redistributeUntilRepeat memory = do
    count <- STRef.newSTRef 0
    seen <- Table.new

    let loop = do
            redistribute memory
            STRef.modifySTRef' count (+ 1)
            frozen <- Vec.freeze memory
            Table.lookup seen frozen >>= \case
                Nothing -> do
                    STRef.readSTRef count >>= Table.insert seen frozen
                    loop
                Just n -> do
                    cycles <- STRef.readSTRef count
                    pure (cycles, cycles - n)

    Vec.freeze memory >>= flip (Table.insert seen) 0
    loop

part1 :: Vector Int -> Int
part1 vector = ST.runST $ do
    frozen <- Vec.thaw vector
    fst <$> redistributeUntilRepeat frozen

part2 :: Vector Int -> Int
part2 vector = ST.runST $ do
    frozen <- Vec.thaw vector
    snd <$> redistributeUntilRepeat frozen

main :: IO ()
main = do
    let memory = Vec.fromList
            [4, 10, 4, 1, 8, 4, 9, 14, 5, 1, 14, 15, 0, 15, 3, 5]
    print $ part1 memory
    print $ part2 memory
