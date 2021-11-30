{-# LANGUAGE OverloadedStrings #-}

module Test13 where

import Control.Lens (_Left, _Right, preview)
import Control.Monad ((>=>))
import Data.Text qualified as Text
import Flow ((.>))
import Linear (V2(V2))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.Megaparsec qualified as Par

import Day13 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

{-# ANN unitTests ("HLINT: ignore" :: String) #-}
unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests"
    [ Tasty.testGroup "straight track" $
        let makeTrack = fmap Text.singleton .> Text.intercalate "\n"
            tickCart dir y = Day13.tickCart (V2 0 y) $ Day13.MkCart
                { Day13.direction = dir
                , Day13.choice = Day13.GoLeft
                }
            tickMaybe dir y = fmap (tickCart dir y) >=> preview _Right
            tick0 = Par.parseMaybe Day13.parseRailway $ makeTrack "|v|||^|"
            tick1 = tickMaybe Day13.South 1 tick0
            tick2 = tickMaybe Day13.North 5 tick1
            tick3 = tickMaybe Day13.South 2 tick2
            tick4 = fmap (tickCart Day13.North 4) tick3 >>= preview _Left
        in  [ HUnit.testCase "tick 0" $
                fmap Day13.prettyRailway tick0 @?= Just (makeTrack "|v|||^|")
            , HUnit.testCase "tick 1" $
                fmap Day13.prettyRailway tick1 @?= Just (makeTrack "||v||^|")
            , HUnit.testCase "tick 2" $
                fmap Day13.prettyRailway tick2 @?= Just (makeTrack "||v|^||")
            , HUnit.testCase "tick 3" $
                fmap Day13.prettyRailway tick3 @?= Just (makeTrack "|||v^||")
            , HUnit.testCase "tick 4" $
                fmap Day13.prettyCrash tick4 @?= Just (makeTrack "|||X|||")
            ]
    , Tasty.testGroup "rings" $
        let ringsText = Text.replace "?" "\\" $ Text.intercalate "\n"
                [ "/->-?        "
                , "|   |  /----?"
                , "| /-+--+-?  |"
                , "| | |  | v  |"
                , "?-+-/  ?-+--/"
                , "  ?------/   "
                ]
            rings = Par.parseMaybe Day13.parseRailway ringsText
        in  [ HUnit.testCase "crash location" $
                fmap (Day13.tickUntilCrash .> Day13.position) rings
                    @?= Just (V2 7 3)
            , HUnit.testCase "crash state" $
                let crashed = Text.replace "?" "\\" $ Text.intercalate "\n"
                        [ "/---?        "
                        , "|   |  /----?"
                        , "| /-+--+-?  |"
                        , "| | |  X |  |"
                        , "?-+-/  ?-+--/"
                        , "  ?------/   "
                        ]
                in  fmap (Day13.tickUntilCrash .> Day13.prettyCrash) rings
                        @?= Just crashed
            ]
    ]
