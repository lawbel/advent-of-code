{-# LANGUAGE TypeApplications #-}

module Test03 where

import Flow ((.>))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck ((===))
import Test.Tasty.QuickCheck qualified as Check

import Day03 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests"
    [ HUnit.testCase "gammaEpsilon" $
        Day03.gammaEpsilon exampleReport @?= Just (22, 9)
    , HUnit.testCase "oxygen" $
        Day03.oxygen exampleReport @?= Just 23
    , HUnit.testCase "co2" $
        Day03.co2 exampleReport @?= Just 10
    ]

propertyTests :: TestTree
propertyTests = Tasty.testGroup "property tests"
    [ Check.testProperty "highestBitSet == floorLog2" $
        \(Check.Positive n) ->
            let floorLog2 = fromIntegral @Int .> logBase @Float 2 .> floor
            in  Day03.highestBitSet n === floorLog2 n
    ]

exampleReport :: [Int]
exampleReport =
    [ 0b00100
    , 0b11110
    , 0b10110
    , 0b10111
    , 0b10101
    , 0b01111
    , 0b00111
    , 0b11100
    , 0b10000
    , 0b11001
    , 0b00010
    , 0b01010
    ]
