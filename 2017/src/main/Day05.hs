{-# LANGUAGE RecordWildCards #-}

module Day05 where

import Common
import Control.Monad.ST (ST)
import Control.Monad.ST qualified as ST
import Data.STRef (STRef)
import Data.STRef qualified as STRef
import Data.Text qualified as Text
import Data.Vector.Unboxed (Vector)
import Data.Vector.Unboxed qualified as Vec
import Data.Vector.Unboxed.Mutable (MVector)
import Data.Vector.Unboxed.Mutable qualified as Vec.M


data List s = MkList
    { list :: MVector s Int
    , ptr :: STRef s Int }

-- | Attempy to perform a jump instruction:
--
--   * if successful, return @Nothing@
--   * in case of failure, return @Just i@ where @i@ is the out-of-bounds
--     index that was attempted to jump to
jump :: List s -> ST s (Maybe Int)
jump ref = ptrInBounds ref >>= \case
    True  -> Nothing <$ jumpInner ref
    False -> Just <$> STRef.readSTRef (ptr ref)

ptrInBounds :: List s -> ST s Bool
ptrInBounds MkList{..} = do
    i <- STRef.readSTRef ptr
    pure $ (0 <= i) && (i < Vec.M.length list)

-- | Mutate the given list by doing a jump; assumes that the @ptr@ of the list
-- is in-bounds.
jumpInner :: List s -> ST s ()
jumpInner MkList{..} = do
    i <- STRef.readSTRef ptr
    offset <- Vec.M.read list i
    Vec.M.modify list (+ 1) i
    STRef.modifySTRef ptr (+ offset)

-- | Attempt to call 'jump' the given @Int@ number of times, returning:
--
--   * @Just@ the first out-of-bounds index that caused us to fail, or
--   * @Nothing@ if all jumps succeeded.
jumps :: Int -> List s -> ST s (Maybe Int)
jumps n maze
    | n > 0 = jump maze >>= \case
        Nothing -> jumps (n - 1) maze
        Just i  -> pure $ Just i
    | otherwise = pure Nothing

-- | Calls 'jump' on the given list as many times as needed, until the @ptr@
-- escapes the bounds of the list. Returns the number of iterations needed to
-- reach this state.
jumpUntilExit :: List s -> ST s Int
jumpUntilExit ref = do
    count <- STRef.newSTRef 0
    let loop = ptrInBounds ref >>= \case
            True -> do
                jumpInner ref
                STRef.modifySTRef' count (+ 1)
                loop
            False -> STRef.readSTRef count
    loop

part1 :: Vector Int -> Int
part1 initial = ST.runST $ do
    list <- MkList <$> Vec.thaw initial <*> STRef.newSTRef 0
    jumpUntilExit list

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-05.txt"
    let list = Vec.fromList (textRead @Int <$> Text.lines text)
    print $ part1 list
