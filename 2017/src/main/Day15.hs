{-# LANGUAGE BangPatterns #-}

module Day15 where

import Data.Bits ((.&.))
import Data.Word (Word64)


newtype Generator = MkGenerator Word64
    deriving (Eq, Ord, Show)

genA :: Generator
genA = MkGenerator 16_807

genB :: Generator
genB = MkGenerator 48_271

modulus :: Word64
modulus = 2_147_483_647

next :: Generator -> Word64 -> Word64
next (MkGenerator factor) n = (n * factor) `mod` modulus

lowest16BitsEq :: Word64 -> Word64 -> Bool
lowest16BitsEq n m = (n .&. mask) == (m .&. mask)
  where
    mask = 0xFFFF

judge :: Int -> (Generator, Word64) -> (Generator, Word64) -> Int
judge limit (gen1, seed1) (gen2, seed2) = go limit 0 seed1 seed2
  where
    go :: Int -> Int -> Word64 -> Word64 -> Int
    go iter !count val1 val2
        | iter > 0 =
            let next1 = next gen1 val1
                next2 = next gen2 val2
                incr = if lowest16BitsEq next1 next2 then count + 1 else count
            in  go (iter - 1) incr next1 next2
        | otherwise = count

part1 :: Word64 -> Word64 -> Int
part1 seedA seedB = judge 40_000_000 (genA, seedA) (genB, seedB)

main :: IO ()
main = do
    print $ part1 seedA seedB
  where
    seedA = 873
    seedB = 583
