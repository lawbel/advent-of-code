{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Day25 where

import Named (NamedF(Arg), type (:!), (!))


input :: (Int, Int)
input = (10604480, 4126658)

modulus :: Int
modulus = 2020_1227

main :: IO ()
main = do
    print $ part1 input

part1 :: (Int, Int) -> Int
part1 (cardPub, doorPub) = encrKey
  where
    cardLoop = invert
        ! #publicKey cardPub
        ! #subject 7
    encrKey = transform
        ! #subject doorPub
        ! #loopSize cardLoop

transform :: "loopSize" :! Int -> "subject" :! Int -> Int
transform (Arg loopSize) (Arg subject) = go loopSize 1
  where
    go :: Int -> Int -> Int
    go iter !acc
        | iter == 0 = acc
        | otherwise =
            let next = step
                    ! #value acc
                    ! #subject subject
            in  go (iter - 1) next

invert :: "publicKey" :! Int -> "subject" :! Int -> Int
invert (Arg publicKey) (Arg subject) = go 0 1
  where
    go :: Int -> Int -> Int
    go !iter acc
        | acc == publicKey = iter
        | otherwise =
            let next = step
                    ! #value acc
                    ! #subject subject
            in  go (iter + 1) next

step :: "value" :! Int -> "subject" :! Int -> Int
step (Arg value) (Arg subject) = (value * subject) `mod` modulus
