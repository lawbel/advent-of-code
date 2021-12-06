{-# LANGUAGE QuasiQuotes #-}

module Test06 where

import Data.Foldable (toList)
import Data.Function ((&))
import Data.List qualified as List
import Data.Text qualified as Text
import Flow ((.>))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.InterpolatedString.Perl6 (qq)

import Day06 qualified
import Common


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests"
    [ oneLanternfish
    , manyLanternfish
    , afterManyDays
    ]

oneLanternfish :: TestTree
oneLanternfish = Tasty.testGroup "one lanternfish"
    [ HUnit.testCase "after one day" $
        afterDays 1 @?= [Day06.mkFish 2]
    , HUnit.testCase "after another day" $
        afterDays 2 @?= [Day06.mkFish 1]
    , HUnit.testCase "after another day" $
        afterDays 3 @?= [Day06.mkFish 0]
    , HUnit.testCase "after another day" $
        afterDays 4 @?= [Day06.MkFish 6 [Day06.mkFish 8]]
    , HUnit.testCase "after another day" $
        afterDays 5 @?= [Day06.MkFish 5 [Day06.mkFish 7]]
    ]
  where
    afterDays = Day06.days .> Day06.execSea @Int Day06.grow [Day06.mkFish 3]

manyLanternfish :: TestTree
manyLanternfish = Tasty.testGroup "many lanternfish" $
    initCase : do
        (days, expected) <- zip [1..] exampleResults
        let nDays = Text.justifyRight justify ' ' (textShow days)
        pure $ HUnit.testCase [qq|after $nDays days|] $
            List.sort (afterDays days) @?= List.sort expected
  where
    justify = length exampleResults & textShow & Text.length
    initState = Day06.mkFish <$> exampleInput
    initCase = HUnit.testCase "initial state" $
        afterDays 0 @?= fmap Day06.timer initState
    afterDays = Day06.days
        .> Day06.execSea Day06.grow initState
        .> concatMap toList

afterManyDays :: TestTree
afterManyDays = Tasty.testGroup "after many days"
    [ HUnit.testCase "after 18 days" $
        Day06.simulate 18 exampleInput @?= 26
    , HUnit.testCase "after 80 days" $
        Day06.simulate 80 exampleInput @?= 5934
    ]

exampleInput :: [Int]
exampleInput = [3,4,3,1,2]

exampleResults :: [[Int]]
exampleResults =
    [ [2,3,2,0,1]
    , [1,2,1,6,0,8]
    , [0,1,0,5,6,7,8]
    , [6,0,6,4,5,6,7,8,8]
    , [5,6,5,3,4,5,6,7,7,8]
    , [4,5,4,2,3,4,5,6,6,7]
    , [3,4,3,1,2,3,4,5,5,6]
    , [2,3,2,0,1,2,3,4,4,5]
    , [1,2,1,6,0,1,2,3,3,4,8]
    , [0,1,0,5,6,0,1,2,2,3,7,8]
    , [6,0,6,4,5,6,0,1,1,2,6,7,8,8,8]
    , [5,6,5,3,4,5,6,0,0,1,5,6,7,7,7,8,8]
    , [4,5,4,2,3,4,5,6,6,0,4,5,6,6,6,7,7,8,8]
    , [3,4,3,1,2,3,4,5,5,6,3,4,5,5,5,6,6,7,7,8]
    , [2,3,2,0,1,2,3,4,4,5,2,3,4,4,4,5,5,6,6,7]
    , [1,2,1,6,0,1,2,3,3,4,1,2,3,3,3,4,4,5,5,6,8]
    , [0,1,0,5,6,0,1,2,2,3,0,1,2,2,2,3,3,4,4,5,7,8]
    , [6,0,6,4,5,6,0,1,1,2,6,0,1,1,1,2,2,3,3,4,6,7,8,8,8,8]
    ]
