{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Test25 where

import Data.Either (fromRight)
import Data.Map (Map)
import Data.Text qualified as Text
import Instances.TH.Lift ()  -- for @instance Lift Map@
import Language.Haskell.TH.Syntax (liftTyped)
import Linear.V2 (V2)
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Day25 (Cucumber)
import Day25 qualified


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests"
    [ rowTests
    , smallTests
    , mediumTests
    , largeTests ]


rowTests :: TestTree
rowTests = Tasty.testGroup "row tests"
    [ HUnit.testCase "after 1 step" $
        uncurry (Day25.moves 1) seaFloorRow @?= $$(
            liftTyped $ snd $ fromRight undefined $
                Day25.getSeaFloor "...>>>>.>.." )
    , HUnit.testCase "after 2 steps" $
        uncurry (Day25.moves 2) seaFloorRow @?= $$(
            liftTyped $ snd $ fromRight undefined $
                Day25.getSeaFloor "...>>>.>.>." ) ]
  where
    seaFloorRow :: (V2 Int, Map (V2 Int) Cucumber)
    seaFloorRow = $$(
        liftTyped $
            fromRight undefined $
                Day25.getSeaFloor "...>>>>>..." )


smallTests :: TestTree
smallTests = Tasty.testGroup "small tests"
    [ HUnit.testCase "after 1 step" $
        uncurry (Day25.moves 1) seaFloorSmall @?= $$(
            liftTyped $ snd $ fromRight undefined $ Day25.getSeaFloor $
                Text.intercalate "\n"
                    [ ".........."
                    , ".>........"
                    , "..v....v>."
                    , ".........." ] ) ]
  where
    seaFloorSmall :: (V2 Int, Map (V2 Int) Cucumber)
    seaFloorSmall = $$(
        liftTyped $ fromRight undefined $
            Day25.getSeaFloor $
                Text.intercalate "\n"
                    [ ".........."
                    , ".>v....v.."
                    , ".......>.."
                    , ".........." ] )


mediumTests :: TestTree
mediumTests = Tasty.testGroup "medium tests"
    [ HUnit.testCase "after 1 step" $
        uncurry (Day25.moves 1) seaFloorMedium @?= $$(
            liftTyped $ snd $ fromRight undefined $ Day25.getSeaFloor $
                Text.intercalate "\n"
                    [ "..vv>.."
                    , "......."
                    , ">......"
                    , "v.....>"
                    , ">......"
                    , "......."
                    , "....v.." ] )
    , HUnit.testCase "after 2 steps" $
        uncurry (Day25.moves 2) seaFloorMedium @?= $$(
            liftTyped $ snd $ fromRight undefined $ Day25.getSeaFloor $
                Text.intercalate "\n"
                    [ "....v>."
                    , "..vv..."
                    , ".>....."
                    , "......>"
                    , "v>....."
                    , "......."
                    , "......." ] )
    , HUnit.testCase "after 3 steps" $
        uncurry (Day25.moves 3) seaFloorMedium @?= $$(
            liftTyped $ snd $ fromRight undefined $ Day25.getSeaFloor $
                Text.intercalate "\n"
                    [ "......>"
                    , "..v.v.."
                    , "..>v..."
                    , ">......"
                    , "..>...."
                    , "v......"
                    , "......." ] )
    , HUnit.testCase "after 4 steps" $
        uncurry (Day25.moves 4) seaFloorMedium @?= $$(
            liftTyped $ snd $ fromRight undefined $ Day25.getSeaFloor $
                Text.intercalate "\n"
                    [ ">......"
                    , "..v...."
                    , "..>.v.."
                    , ".>.v..."
                    , "...>..."
                    , "......."
                    , "v......" ] ) ]
  where
    seaFloorMedium :: (V2 Int, Map (V2 Int) Cucumber)
    seaFloorMedium = $$(
        liftTyped $ fromRight undefined $
            Day25.getSeaFloor $
                Text.intercalate "\n"
                    [ "...>..."
                    , "......."
                    , "......>"
                    , "v.....>"
                    , "......>"
                    , "......."
                    , "..vvv.." ] )


largeTests :: TestTree
largeTests = Tasty.testGroup "large tests"
    [ HUnit.testCase "after 1 step" $
        uncurry (Day25.moves 1) seaFloorLarge @?= $$(
            liftTyped $ snd $ fromRight undefined $ Day25.getSeaFloor $
                Text.intercalate "\n"
                    [ "....>.>v.>"
                    , "v.v>.>v.v."
                    , ">v>>..>v.."
                    , ">>v>v>.>.v"
                    , ".>v.v...v."
                    , "v>>.>vvv.."
                    , "..v...>>.."
                    , "vv...>>vv."
                    , ">.v.v..v.v" ] )
    , HUnit.testCase "after 5 steps" $
        uncurry (Day25.moves 5) seaFloorLarge @?= $$(
            liftTyped $ snd $ fromRight undefined $ Day25.getSeaFloor $
                Text.intercalate "\n"
                    [ "vv>...>v>."
                    , "v.v.v>.>v."
                    , ">.v.>.>.>v"
                    , ">v>.>..v>>"
                    , "..v>v.v..."
                    , "..>.>>vvv."
                    , ".>...v>v.."
                    , "..v.v>>v.v"
                    , "v.v.>...v." ] )
    , HUnit.testCase "after 10 steps" $
        uncurry (Day25.moves 10) seaFloorLarge @?= $$(
            liftTyped $ snd $ fromRight undefined $ Day25.getSeaFloor $
                Text.intercalate "\n"
                    [ "..>..>>vv."
                    , "v.....>>.v"
                    , "..v.v>>>v>"
                    , "v>.>v.>>>."
                    , "..v>v.vv.v"
                    , ".v.>>>.v.."
                    , "v.v..>v>.."
                    , "..v...>v.>"
                    , ".vv..v>vv." ] )
    , HUnit.testCase "after 20 steps" $
        uncurry (Day25.moves 20) seaFloorLarge @?= $$(
            liftTyped $ snd $ fromRight undefined $ Day25.getSeaFloor $
                Text.intercalate "\n"
                    [ "v>.....>>."
                    , ">vv>.....v"
                    , ".>v>v.vv>>"
                    , "v>>>v.>v.>"
                    , "....vv>v.."
                    , ".v.>>>vvv."
                    , "..v..>>vv."
                    , "v.v...>>.v"
                    , "..v.....v>" ] )
    , HUnit.testCase "after 30 steps" $
        uncurry (Day25.moves 30) seaFloorLarge @?= $$(
            liftTyped $ snd $ fromRight undefined $ Day25.getSeaFloor $
                Text.intercalate "\n"
                    [ ".vv.v..>>>"
                    , "v>...v...>"
                    , ">.v>.>vv.>"
                    , ">v>.>.>v.>"
                    , ".>..v.vv.."
                    , "..v>..>>v."
                    , "....v>..>v"
                    , "v.v...>vv>"
                    , "v.v...>vvv" ] )
    , HUnit.testCase "after 40 steps" $
        uncurry (Day25.moves 40) seaFloorLarge @?= $$(
            liftTyped $ snd $ fromRight undefined $ Day25.getSeaFloor $
                Text.intercalate "\n"
                    [ ">>v>v..v.."
                    , "..>>v..vv."
                    , "..>>>v.>.v"
                    , "..>>>>vvv>"
                    , "v.....>..."
                    , "v.v...>v>>"
                    , ">vv.....v>"
                    , ".>v...v.>v"
                    , "vvv.v..v.>" ] )
    , HUnit.testCase "after 50 steps" $
        uncurry (Day25.moves 50) seaFloorLarge @?= $$(
            liftTyped $ snd $ fromRight undefined $ Day25.getSeaFloor $
                Text.intercalate "\n"
                    [ "..>>v>vv.v"
                    , "..v.>>vv.."
                    , "v.>>v>>v.."
                    , "..>>>>>vv."
                    , "vvv....>vv"
                    , "..v....>>>"
                    , "v>.......>"
                    , ".vv>....v>"
                    , ".>v.vv.v.." ] )
    , HUnit.testCase "after 56 steps" $
        uncurry (Day25.moves 56) seaFloorLarge @?= $$(
            liftTyped $ snd $ fromRight undefined $ Day25.getSeaFloor $
                Text.intercalate "\n"
                    [ "..>>v>vv.."
                    , "..v.>>vv.."
                    , "..>>v>>vv."
                    , "..>>>>>vv."
                    , "v......>vv"
                    , "v>v....>>v"
                    , "vvv....>.>"
                    , ">vv......>"
                    , ".>v.vv.v.." ] )
    , HUnit.testCase "after 57 steps" $
        uncurry (Day25.moves 57) seaFloorLarge @?= $$(
            liftTyped $ snd $ fromRight undefined $ Day25.getSeaFloor $
                Text.intercalate "\n"
                    [ "..>>v>vv.."
                    , "..v.>>vv.."
                    , "..>>v>>vv."
                    , "..>>>>>vv."
                    , "v......>vv"
                    , "v>v....>>v"
                    , "vvv.....>>"
                    , ">vv......>"
                    , ".>v.vv.v.." ] )
    , HUnit.testCase "after 58 steps" $
        uncurry (Day25.moves 58) seaFloorLarge @?= $$(
            liftTyped $ snd $ fromRight undefined $ Day25.getSeaFloor $
                Text.intercalate "\n"
                    [ "..>>v>vv.."
                    , "..v.>>vv.."
                    , "..>>v>>vv."
                    , "..>>>>>vv."
                    , "v......>vv"
                    , "v>v....>>v"
                    , "vvv.....>>"
                    , ">vv......>"
                    , ".>v.vv.v.." ] )
    , HUnit.testCase "stops at 57 steps" $
        let result :: Map (V2 Int) Cucumber = $$( liftTyped $ snd $
                fromRight undefined $ Day25.getSeaFloor $ Text.intercalate "\n"
                    [ "..>>v>vv.."
                    , "..v.>>vv.."
                    , "..>>v>>vv."
                    , "..>>>>>vv."
                    , "v......>vv"
                    , "v>v....>>v"
                    , "vvv.....>>"
                    , ">vv......>"
                    , ".>v.vv.v.." ] )
        in  uncurry Day25.moveFixpoint seaFloorLarge @?= (57, result) ]
  where
    seaFloorLarge :: (V2 Int, Map (V2 Int) Cucumber)
    seaFloorLarge = $$(
        liftTyped $ fromRight undefined $
            Day25.getSeaFloor $
                Text.intercalate "\n"
                    [ "v...>>.vv>"
                    , ".vv>>.vv.."
                    , ">>.>v>...v"
                    , ">>v>>.>.v."
                    , "v>v.vv.v.."
                    , ">.>>..v..."
                    , ".vv..>.>v."
                    , "v.v..>>v.v"
                    , "....v..v.>" ] )
