{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
　
module Test10 where

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text qualified as Text
import Day10 (Point)
import Flow ((.>))
import Linear (V2(V2))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.Megaparsec qualified as Par

import Day10 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [parseTests, unitTests]

parseTests :: TestTree
parseTests = Tasty.testGroup "parse tests"
    [ HUnit.testCase "example" $
        let text = Text.unlines
                [ "position=< 9,  1> velocity=< 0,  2>"
                , "position=< 7,  0> velocity=<-1,  0>"
                , "position=< 3, -2> velocity=<-1,  1>"
                , "position=< 6, 10> velocity=<-2, -1>"
                , "position=< 2, -4> velocity=< 2,  2>"
                , "position=<-6, 10> velocity=< 2, -2>"
                , "position=< 1,  8> velocity=< 1, -1>"
                , "position=< 1,  7> velocity=< 1,  0>"
                , "position=<-3, 11> velocity=< 1, -2>"
                , "position=< 7,  6> velocity=<-1, -1>"
                , "position=<-2,  3> velocity=< 1,  0>"
                , "position=<-4,  3> velocity=< 2,  0>"
                , "position=<10, -3> velocity=<-1,  1>"
                , "position=< 5, 11> velocity=< 1, -2>"
                , "position=< 4,  7> velocity=< 0, -1>"
                , "position=< 8, -2> velocity=< 0,  1>"
                , "position=<15,  0> velocity=<-2,  0>"
                , "position=< 1,  6> velocity=< 1,  0>"
                , "position=< 8,  9> velocity=< 0, -1>"
                , "position=< 3,  3> velocity=<-1,  1>"
                , "position=< 0,  5> velocity=< 0, -1>"
                , "position=<-2,  2> velocity=< 2,  0>"
                , "position=< 5, -2> velocity=< 1,  2>"
                , "position=< 1,  4> velocity=< 2,  1>"
                , "position=<-2,  7> velocity=< 2, -2>"
                , "position=< 3,  6> velocity=<-1, -1>"
                , "position=< 5,  0> velocity=< 1,  0>"
                , "position=<-6,  0> velocity=< 2,  0>"
                , "position=< 5,  9> velocity=< 1, -2>"
                , "position=<14,  7> velocity=<-2,  0>"
                , "position=<-3,  6> velocity=< 2, -1>"
                ]
        in  Par.parseMaybe Day10.parsePoints text @?= Just points
    ]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests"
    [ HUnit.testCase "step" $
        let point = makePoint @Int (3, 9) (1, -2)
        in  Day10.position (iter 3 Day10.step point) @?= V2 6 3
    , HUnit.testCase "stepWhileDiamDecr" $
        let result = Text.intercalate "\n"
                [ "#...#..###"
                , "#...#...#."
                , "#...#...#."
                , "#####...#."
                , "#...#...#."
                , "#...#...#."
                , "#...#...#."
                , "#...#..###"
                ]
            finalPos = Day10.position <$> Day10.stepWhileDiamDecr points
        in  Day10.vizualize finalPos @?= result
    ]

iter :: Int -> (a -> a) -> a -> a
iter n f
    | n <= 0    = id
    | otherwise = f .> iter (n - 1) f

points :: NonEmpty (Point Int)
points =
      makePoint (9, 1) (0, 2) :|
    [ makePoint (7, 0) (-1, 0)
    , makePoint (3, -2) (-1, 1)
    , makePoint (6, 10) (-2, -1)
    , makePoint (2, -4) (2, 2)
    , makePoint (-6, 10) (2, -2)
    , makePoint (1, 8) (1, -1)
    , makePoint (1, 7) (1, 0)
    , makePoint (-3, 11) (1, -2)
    , makePoint (7, 6) (-1, -1)
    , makePoint (-2, 3) (1, 0)
    , makePoint (-4, 3) (2, 0)
    , makePoint (10, -3) (-1, 1)
    , makePoint (5, 11) (1, -2)
    , makePoint (4, 7) (0, -1)
    , makePoint (8, -2) (0, 1)
    , makePoint (15, 0) (-2, 0)
    , makePoint (1, 6) (1, 0)
    , makePoint (8, 9) (0, -1)
    , makePoint (3, 3) (-1, 1)
    , makePoint (0, 5) (0, -1)
    , makePoint (-2, 2) (2, 0)
    , makePoint (5, -2) (1, 2)
    , makePoint (1, 4) (2, 1)
    , makePoint (-2, 7) (2, -2)
    , makePoint (3, 6) (-1, -1)
    , makePoint (5, 0) (1, 0)
    , makePoint (-6, 0) (2, 0)
    , makePoint (5, 9) (1, -2)
    , makePoint (14, 7) (-2, 0)
    , makePoint (-3, 6) (2, -1)
    ]

makePoint :: (a, a) -> (a, a) -> Point a
makePoint (a, b) (c, d) = Day10.MkPoint
    { Day10.position = V2 a b
    , Day10.velocity = V2 c d
    }
