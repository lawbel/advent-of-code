{-# LANGUAGE OverloadedStrings #-}

module Test13 where

import Control.Lens (_Left, _Right, preview)
import Control.Monad.State qualified as State
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as List.NE
import Data.Map qualified as Map
import Data.Text qualified as Text
import Flow ((.>))
import Linear (V2(V2))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Text.Megaparsec qualified as Par

import Day13 (Cart, Crashes, Railway)
import Day13 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

tickCart :: V2 Int -> Cart -> Railway -> Either Crashes Railway
tickCart vec cart railway = case err of
    Nothing    -> Right updated
    Just crash -> Left $ Day13.MkCrashes (crash :| []) updated
  where
    (err, updated) = State.runState (Day13.tickCart vec cart) railway

{-# ANN unitTests ("HLINT: ignore" :: String) #-}
unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests"
    [ Tasty.testGroup "straight track" $
        let makeTrack = fmap Text.singleton .> Text.intercalate "\n"
            stepCart dir y = tickCart (V2 0 y) $ Day13.MkCart dir Day13.GoLeft
            step0 = Par.parseMaybe Day13.parseRailway $ makeTrack "|v|||^|"
            step1 = fmap (stepCart Day13.South 1) step0 >>= preview _Right
            step2 = fmap (stepCart Day13.North 5) step1 >>= preview _Right
            step3 = fmap (stepCart Day13.South 2) step2 >>= preview _Right
            step4 = fmap (stepCart Day13.North 4) step3 >>= preview _Left
        in  [ HUnit.testCase "step 0" $
                fmap Day13.prettyRailway step0 @?= Just (makeTrack "|v|||^|")
            , HUnit.testCase "step 1" $
                fmap Day13.prettyRailway step1 @?= Just (makeTrack "||v||^|")
            , HUnit.testCase "step 2" $
                fmap Day13.prettyRailway step2 @?= Just (makeTrack "||v|^||")
            , HUnit.testCase "step 3" $
                fmap Day13.prettyRailway step3 @?= Just (makeTrack "|||v^||")
            , HUnit.testCase "step 4" $
                fmap Day13.prettyCrash step4 @?= Just (makeTrack "|||X|||")
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
            crashPositions = Day13.tickUntilCrash
                .> Day13.crashes
                .> fmap Day13.position
                .> List.NE.toList
        in  [ HUnit.testCase "crash location" $
                fmap crashPositions rings @?= Just [V2 7 3]
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
    , Tasty.testGroup "figure eight" $
        [ HUnit.testCase "tickUntilCarts" $
            let eightText = Text.replace "?" "\\" $ Text.intercalate "\n"
                    [ "/>-<?  "
                    , "|   |  "
                    , "| /<+-?"
                    , "| | | v"
                    , "?>+</ |"
                    , "  |   ^"
                    , "  ?<->/"
                    ]
                eight = Par.parseMaybe Day13.parseRailway eightText
                tick = Day13.tickUntilCarts 1 .> Day13.railCarts .> Map.keys
            in  fmap tick eight @?= Just [V2 6 4]
        ]
    ]
