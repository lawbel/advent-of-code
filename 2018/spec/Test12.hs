{-# LANGUAGE OverloadedStrings #-}

module Test12 where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Linear (V2(V2))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.Megaparsec qualified as Par

import Day12 (Plant, Match)
import Day12 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [parseTests, prettyTests, unitTests]

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
        in  Par.parseMaybe Day12.parseInput text @?= Just (initState, rules)
    ]

prettyTests :: TestTree
prettyTests = Tasty.testGroup "pretty tests"
    [ HUnit.testCase "prettySteps" $
        let actual = Day12.prettySteps 20 rules Day12.Dead $
                Day12.asIntMap initState
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
        Day12.sumIndexAliveAfter 20 initState rules @?= 325
    ]

initState :: [Plant]
initState =
    [ a, d, d, a, d, a, d, d, a, a, d, d, d
    , d, d, d, a, a, a, d, d, d, a, a, a
    ]
  where
    a = Day12.Alive
    d = Day12.Dead

rules :: Map (Match Plant) Plant
rules = Map.fromList
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
