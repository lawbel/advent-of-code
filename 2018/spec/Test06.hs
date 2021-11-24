{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Test06 where

import Data.Char qualified as Char
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as List.NE
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, maybeToList)
import Data.Text qualified as Text
import Data.Tuple (swap)
import Flow ((.>))
import Linear (V2(V2))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.Megaparsec qualified as Par

import Day06 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [parseTests, prettyTests, unitTests]

coords :: NonEmpty (V2 Int, Char)
coords =
      (V2 1 1, 'A') :|
    [ (V2 1 6, 'B')
    , (V2 8 3, 'C')
    , (V2 3 4, 'D')
    , (V2 5 5, 'E')
    , (V2 8 9, 'F')
    ]

parseTests :: TestTree
parseTests = Tasty.testGroup "parse tests"
    [ HUnit.testCase "example" $
        let text = "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9"
        in  Par.parseMaybe Day06.parseCoords text @?= Just (fst <$> coords)
    ]

prettyTests :: TestTree
prettyTests = Tasty.testGroup "prettyTests tests"
    [ HUnit.testCase "points only" $
        let box = Day06.MkBox
                { Day06.minimal = V2 0 0
                , Day06.maximal = V2 9 9
                }
            assocs = Map.fromList $ List.NE.toList coords
            vizualize = fromMaybe '.'
            result = Text.intercalate "\n"
                [ ".........."
                , ".A........"
                , ".........."
                , "........C."
                , "...D......"
                , ".....E...."
                , ".B........"
                , ".........."
                , ".........."
                , "........F."
                ]
        in  Day06.vizualizeWithin box assocs vizualize @?= result
    , HUnit.testCase "points + closest" $
        let box = Day06.MkBox
                { Day06.minimal = V2 0 0
                , Day06.maximal = V2 9 9
                }
            assocs = Map.fromList $ List.NE.toList coords
            closest = Day06.closestWithin box assocs
            vizualize = \case
                Just (0, c :| []) -> c
                Just (_, c :| []) -> Char.toLower c
                _                 -> '.'
            result = Text.intercalate "\n"
                [ "aaaaa.cccc"
                , "aAaaa.cccc"
                , "aaaddecccc"
                , "aadddeccCc"
                , "..dDdeeccc"
                , "bb.deEeecc"
                , "bBb.eeee.."
                , "bbb.eeefff"
                , "bbb.eeffff"
                , "bbb.ffffFf"
                ]
        in  Day06.vizualizeWithin box closest vizualize @?= result
    ]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests"
    [ Tasty.testGroup "countWithin" $
        let box = Day06.MkBox
                { Day06.minimal = V2 0 0
                , Day06.maximal = V2 9 9
                }
            assocs = Map.fromList $ List.NE.toList coords
            closest = Day06.closestWithin box assocs
            getTag = snd .> \case
                c :| [] -> Just c
                _       -> Nothing
            result = Day06.countWithin box $ Map.mapMaybe getTag closest
        in  [ HUnit.testCase "point D" $ result Map.!? 'D' @?= Just 9
            , HUnit.testCase "point E" $ result Map.!? 'E' @?= Just 17
            ]
    , Tasty.testGroup "boxEnclosing + onBoundary" $
        let box = Day06.boxEnclosing (fst <$> coords)
            swapped = Map.fromList $ List.NE.toList $ fmap swap coords
            isOnBoundary c = Day06.onBoundary box <$> swapped Map.!? c
        in  [ HUnit.testCase "point A" $ isOnBoundary 'A' @?= Just True
            , HUnit.testCase "point B" $ isOnBoundary 'B' @?= Just True
            , HUnit.testCase "point C" $ isOnBoundary 'C' @?= Just True
            , HUnit.testCase "point D" $ isOnBoundary 'D' @?= Just False
            , HUnit.testCase "point E" $ isOnBoundary 'E' @?= Just False
            , HUnit.testCase "point F" $ isOnBoundary 'F' @?= Just True
            ]
    , HUnit.testCase "finiteAreasInBoxEnclosing" $
        let areas = Day06.finiteAreasInBoxEnclosing (fst <$> coords)
            swapped = Map.fromList $ List.NE.toList $ fmap swap coords
            makeMap assoc = Map.fromList $ do
                (char, val) <- assoc
                key <- maybeToList $ swapped Map.!? char
                pure (key, val)
        in  areas @?= makeMap [('D', 9), ('E', 17)]
    ]
