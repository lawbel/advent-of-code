{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test12 where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Linear (V2(V2))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck (Arbitrary)
import Test.Tasty.QuickCheck qualified as Check
import Text.Megaparsec qualified as Par

import Day12 (Plant, Match)
import Day12 qualified


instance Arbitrary Plant where
    arbitrary = Check.oneof
        [ pure Day12.Dead
        , pure Day12.Alive
        ]

instance Arbitrary a => Arbitrary (Match a) where
    arbitrary = do
        lefts <- V2 <$> Check.arbitrary <*> Check.arbitrary
        center <- Check.arbitrary
        rights <- V2 <$> Check.arbitrary <*> Check.arbitrary
        pure $ Day12.MkMatch{..}

main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests"
    [ parseTests
    , prettyTests
    , unitTests
    , propertyTests
    ]

parseTests :: TestTree
parseTests = Tasty.testGroup "parse tests"
    [ HUnit.testCase "parseInput" $
        let text = Text.intercalate "\n"
                [ "initial state: #..#.#..##......###...###"
                , ""
                , "...## => #"
                , "..#.. => #"
                , ".#... => #"
                , ".#.#. => #"
                , ".#.## => #"
                , ".##.. => #"
                , ".#### => #"
                , "#.#.# => #"
                , "#.### => #"
                , "##.#. => #"
                , "##.## => #"
                , "###.. => #"
                , "###.# => #"
                , "####. => #"
                ]
        in  Par.parseMaybe Day12.parseInput text
                @?= Just (examplePlants, exampleRules)
    ]

prettyTests :: TestTree
prettyTests = Tasty.testGroup "pretty tests"
    [ HUnit.testCase "prettySteps" $
        let actual = Day12.prettySteps 20 exampleRules Day12.Dead $
                Day12.asIntMap examplePlants
            expected = Text.intercalate "\n"
                [ "                 1         2         3     "
                , "       0         0         0         0     "
                , " 0: ...#..#.#..##......###...###..........."
                , " 1: ...#...#....#.....#..#..#..#..........."
                , " 2: ...##..##...##....#..#..#..##.........."
                , " 3: ..#.#...#..#.#....#..#..#...#.........."
                , " 4: ...#.#..#...#.#...#..#..##..##........."
                , " 5: ....#...##...#.#..#..#...#...#........."
                , " 6: ....##.#.#....#...#..##..##..##........"
                , " 7: ...#..###.#...##..#...#...#...#........"
                , " 8: ...#....##.#.#.#..##..##..##..##......."
                , " 9: ...##..#..#####....#...#...#...#......."
                , "10: ..#.#..#...#.##....##..##..##..##......"
                , "11: ...#...##...#.#...#.#...#...#...#......"
                , "12: ...##.#.#....#.#...#.#..##..##..##....."
                , "13: ..#..###.#....#.#...#....#...#...#....."
                , "14: ..#....##.#....#.#..##...##..##..##...."
                , "15: ..##..#..#.#....#....#..#.#...#...#...."
                , "16: .#.#..#...#.#...##...#...#.#..##..##..."
                , "17: ..#...##...#.#.#.#...##...#....#...#..."
                , "18: ..##.#.#....#####.#.#.#...##...##..##.."
                , "19: .#..###.#..#.#.#######.#.#.#..#.#...#.."
                , "20: .#....##....#####...#######....#.#..##."
                ]
        in  actual @?= expected
    ]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests"
    [ HUnit.testCase "sumIndexAliveAfter" $
        Day12.sumIndexAliveAfter 20 examplePlants exampleRules @?= 325
    ]

propertyTests :: TestTree
propertyTests = Tasty.testGroup "property tests"
    [ Check.testProperty "sumIndexAliveAfter' == sumIndexAliveAfter" $
        \(Check.NonNegative n) plants rules ->
            let naive = Day12.sumIndexAliveAfter n plants rules
                optimized = Day12.sumIndexAliveAfter' n plants rules
            in  fromIntegral naive == optimized
    ]

examplePlants :: [Plant]
examplePlants =
    [ a, d, d, a, d, a, d, d, a, a, d, d, d
    , d, d, d, a, a, a, d, d, d, a, a, a
    ]
  where
    a = Day12.Alive
    d = Day12.Dead

exampleRules :: Map (Match Plant) Plant
exampleRules = Map.fromList
    [ (Day12.MkMatch (V2 d d) d (V2 a a), a)
    , (Day12.MkMatch (V2 d d) a (V2 d d), a)
    , (Day12.MkMatch (V2 d a) d (V2 d d), a)
    , (Day12.MkMatch (V2 d a) d (V2 a d), a)
    , (Day12.MkMatch (V2 d a) d (V2 a a), a)
    , (Day12.MkMatch (V2 d a) a (V2 d d), a)
    , (Day12.MkMatch (V2 d a) a (V2 a a), a)
    , (Day12.MkMatch (V2 a d) a (V2 d a), a)
    , (Day12.MkMatch (V2 a d) a (V2 a a), a)
    , (Day12.MkMatch (V2 a a) d (V2 a d), a)
    , (Day12.MkMatch (V2 a a) d (V2 a a), a)
    , (Day12.MkMatch (V2 a a) a (V2 d d), a)
    , (Day12.MkMatch (V2 a a) a (V2 d a), a)
    , (Day12.MkMatch (V2 a a) a (V2 a d), a)
    ]
  where
    a = Day12.Alive
    d = Day12.Dead
