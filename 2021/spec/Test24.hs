{-# LANGUAGE OverloadedLabels #-}

module Test24 where

import Data.Bits (Bits, (.&.), shiftR, bit)
import Optics ((^?), (%), _Right)
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.QuickCheck ((===))
import Test.Tasty.QuickCheck qualified as Check

import Day24 (Instr, Register)
import Day24 qualified



main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [propertyTests]

propertyTests :: TestTree
propertyTests = Tasty.testGroup "property tests"
    [ Check.testProperty "prog1 == negate" $ \a ->
        let result = Day24.compute [a] prog1 ^? _Right % #x
        in  result === Just (-a)
    , Check.testProperty "prog2 == (\\a b -> b == 3a)" $ \a b ->
        let result = Day24.compute [a, b] prog2 ^? _Right % #z
        in  result === Just (if b == 3*a then 1 else 0)
    , Check.testProperty "prog3 == intToBin" $ \(Check.NonNegative a) ->
        let result = do
                Day24.MkRegisters w x y z <- Day24.compute [a] prog3 ^? _Right
                pure [z, y, x, w]
        in  result === Just [ getBit n a | n <- [0..3] ] ]

getBit :: Bits a => Int -> a -> a
getBit n val = shiftR (val .&. bit n) n

prog1 :: [Instr Register Int]
prog1 =
    [ Day24.Inp Day24.X
    , Day24.Mul Day24.X $ Right (-1) ]

prog2 :: [Instr Register Int]
prog2 =
    [ Day24.Inp Day24.Z
    , Day24.Inp Day24.X
    , Day24.Mul Day24.Z $ Right 3
    , Day24.Eql Day24.Z $ Left Day24.X ]

prog3 :: [Instr Register Int]
prog3 =
    [ Day24.Inp Day24.W
    , Day24.Add Day24.Z $ Left Day24.W
    , Day24.Mod Day24.Z $ Right 2
    , Day24.Div Day24.W $ Right 2
    , Day24.Add Day24.Y $ Left Day24.W
    , Day24.Mod Day24.Y $ Right 2
    , Day24.Div Day24.W $ Right 2
    , Day24.Add Day24.X $ Left Day24.W
    , Day24.Mod Day24.X $ Right 2
    , Day24.Div Day24.W $ Right 2
    , Day24.Mod Day24.W $ Right 2 ]
