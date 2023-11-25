{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test05 where

import Data.Char (isLetter)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck (Arbitrary)
import Test.Tasty.QuickCheck qualified as Check

import Day05 qualified


instance Arbitrary Text where
    arbitrary =
        let letterChar = Check.arbitraryBoundedEnum `Check.suchThat` isLetter
        in  Text.pack <$> Check.listOf letterChar

main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [propertyTests, unitTests]

propertyTests :: TestTree
propertyTests = Tasty.testGroup "properties"
    [ Check.testProperty "fixed trigger == reactFully" $
        \text -> fixed Day05.trigger text == Day05.reactFully text
    , Check.testProperty "fixed react == reactFully" $
        \text -> fixed Day05.react text == Day05.reactFully text
    ]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests"
    [ triggerTests
    , reactTests
    , reduceTests
    ]

triggerTests :: TestTree
triggerTests = Tasty.testGroup "trigger tests"
    [ HUnit.testCase "example 1" $ Day05.trigger poly1 @?= poly2
    , HUnit.testCase "example 2" $ Day05.trigger poly2 @?= poly3
    , HUnit.testCase "example 3" $ Day05.trigger poly3 @?= poly4
    , HUnit.testCase "example 4" $ Day05.trigger poly4 @?= poly4
    ]

reactTests :: TestTree
reactTests = Tasty.testGroup "react tests"
    [ HUnit.testCase "example 1" $ Day05.reactFully poly1 @?= poly4
    ]

reduceTests :: TestTree
reduceTests = Tasty.testGroup "reduce tests"
    [ HUnit.testCase "units" $ List.sort (Day05.units poly1) @?= ['a' .. 'd']
    , Tasty.testGroup "unit a" $
        let without = "dbcCCBcCcD"
        in  [ HUnit.testCase "init" $ Day05.without 'a' poly1 @?= without
            , HUnit.testCase "result" $ Day05.reactFully without @?= "dbCBcD"
            ]
    , Tasty.testGroup "unit b" $
        let without = "daAcCaCAcCcaDA"
        in  [ HUnit.testCase "init" $ Day05.without 'b' poly1 @?= without
            , HUnit.testCase "result" $ Day05.reactFully without @?= "daCAcaDA"
            ]
    , Tasty.testGroup "unit c" $
        let without = "dabAaBAaDA"
        in  [ HUnit.testCase "init" $ Day05.without 'c' poly1 @?= without
            , HUnit.testCase "result" $ Day05.reactFully without @?= "daDA"
            ]
    , Tasty.testGroup "unit d" $
        let without = "abAcCaCBAcCcaA"
        in  [ HUnit.testCase "init" $ Day05.without 'd' poly1 @?= without
            , HUnit.testCase "result" $ Day05.reactFully without @?= "abCBAc"
            ]
    ]

poly1 :: Text
poly1 = "dabAcCaCBAcCcaDA"

poly2 :: Text
poly2 = "dabAaCBAcCcaDA"

poly3 :: Text
poly3 = "dabCBAcCcaDA"

poly4 :: Text
poly4 = "dabCBAcaDA"

fixed :: Eq a => (a -> a) -> a -> a
fixed f x = let y = f x in if y == x then y else fixed f y
