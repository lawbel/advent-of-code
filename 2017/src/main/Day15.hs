{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Day15 where

import Data.Bits ((.&.))
import Data.Word (Word64)


data Generator = MkGenerator
    { factor :: Word64
    , criteria :: Word64 -> Bool }

genA :: Generator
genA = MkGenerator
    { factor = 16_807
    , criteria = const True }

genB :: Generator
genB = MkGenerator
    { factor = 48_271
    , criteria = const True }

genA' :: Generator
genA' = genA {criteria = \n -> n `mod` 4 == 0}

genB' :: Generator
genB' = genB {criteria = \n -> n `mod` 8 == 0}

modulus :: Word64
modulus = 2_147_483_647

next :: Generator -> Word64 -> Word64
next gen@MkGenerator{..} n
    | criteria m = m
    | otherwise  = next gen m
  where
    m = (n * factor) `mod` modulus

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

part2 :: Word64 -> Word64 -> Int
part2 seedA seedB = judge 5_000_000 (genA', seedA) (genB', seedB)

main :: IO ()
main = do
    print $ part1 seedA seedB
    print $ part2 seedA seedB
  where
    seedA = 873
    seedB = 583
