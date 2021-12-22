{-# LANGUAGE OverloadedStrings #-}

module Test22 where

import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Linear (V3(V3))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.Megaparsec qualified as Par

import Day22 (Setting)
import Day22 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [parseTests, unitTests]

parseTests :: TestTree
parseTests = Tasty.testGroup "parse tests"
    [ HUnit.testCase "example" $
        Par.parseMaybe Day22.parseCuboids exampleText1 @?= Just example1 ]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [sizeTests]

sizeTests :: TestTree
sizeTests = Tasty.testGroup "size"
    [ HUnit.testCase "example one" $
        Set.size (Day22.applyAll example1) @?= 39
    , HUnit.testCase "example two" $
        Set.size (Day22.applyAll $ Day22.wrapWithin 50 example2) @?= 590_784 ]

exampleText1 :: Text
exampleText1 = Text.unlines
    [ "on x=10..12,y=10..12,z=10..12"
    , "on x=11..13,y=11..13,z=11..13"
    , "off x=9..11,y=9..11,z=9..11"
    , "on x=10..10,y=10..10,z=10..10" ]

example1 :: [(Setting, V3 Int, V3 Int)]
example1 =
    [ (Day22.On, V3 10 10 10, V3 12 12 12)
    , (Day22.On, V3 11 11 11, V3 13 13 13)
    , (Day22.Off, V3 9 9 9, V3 11 11 11)
    , (Day22.On, V3 10 10 10, V3 10 10 10) ]

example2 :: [(Setting, V3 Int, V3 Int)]
example2 =
    [ (Day22.On, V3 (-20) (-36) (-47), V3 26 17 7)
    , (Day22.On, V3 (-20) (-21) (-26), V3 33 23 28)
    , (Day22.On, V3 (-22) (-29) (-38), V3 28 23 16)
    , (Day22.On, V3 (-46) (-6) (-50), V3 7 46 (-1))
    , (Day22.On, V3 (-49) (-3) (-24), V3 1 46 28)
    , (Day22.On, V3 2 (-22) (-23), V3 47 22 27)
    , (Day22.On, V3 (-27) (-28) (-21), V3 23 26 29)
    , (Day22.On, V3 (-39) (-6) (-3), V3 5 47 44)
    , (Day22.On, V3 (-30) (-8) (-13), V3 21 43 34)
    , (Day22.On, V3 (-22) (-27) (-29), V3 26 20 19)
    , (Day22.Off, V3 (-48) 26 (-47), V3 (-32) 41 (-37))
    , (Day22.On, V3 (-12) 6 (-50), V3 35 50 (-2))
    , (Day22.Off, V3 (-48) (-32) (-15), V3 (-32) (-16) (-5))
    , (Day22.On, V3 (-18) (-33) (-7), V3 26 15 46)
    , (Day22.Off, V3 (-40) (-38) 23, V3 (-22) (-28) 41)
    , (Day22.On, V3 (-16) (-41) (-47), V3 35 10 6)
    , (Day22.Off, V3 (-32) 11 (-14), V3 (-23) 30 3)
    , (Day22.On, V3 (-49) (-3) (-29), V3 (-5) 45 18)
    , (Day22.Off, V3 18 (-20) (-3), V3 30 (-8) 13)
    , (Day22.On, V3 (-41) (-7) (-33), V3 9 43 15)
    , (Day22.On, V3 (-54112) (-85059) (-27449), V3 (-39298) (-49293) 7877)
    , (Day22.On, V3 967 45373 27513, V3 23432 81175 53682) ]
