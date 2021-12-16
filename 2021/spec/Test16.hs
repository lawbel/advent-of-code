{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test16 where

import Data.Either (fromRight)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text (Text)
import Language.Haskell.TH.Syntax (liftTyped)
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit

import Day16 (Packet)
import Day16 qualified
import Common


main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests" [unitTests]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests" [decodingTests, sumVersionTests]

decodingTests :: TestTree
decodingTests = Tasty.testGroup "decoding"
    [ HUnit.testCase "hex1" $
        let packet = Day16.PacketL $ Day16.MkLiteral
                { Day16.litVersion = [Day16.B1, Day16.B1, Day16.B0]
                , Day16.litTypeID = [Day16.B1, Day16.B0, Day16.B0]
                , Day16.litValue =
                    [ Day16.B0, Day16.B1, Day16.B1, Day16.B1
                    , Day16.B1, Day16.B1, Day16.B1, Day16.B0
                    , Day16.B0, Day16.B1, Day16.B0, Day16.B1 ] }
        in  Day16.getPacket hex1 @?= Right packet
    , HUnit.testCase "hex2" $
        let packet = Day16.PacketO $ Day16.MkOperator
                { Day16.opVersion = 1
                , Day16.opTypeID = 6
                , Day16.opLenTypeId = 0
                , Day16.opLength = 27
                , Day16.opPackets = sub1 :| [sub2] }
            sub1 = Day16.PacketL $ Day16.MkLiteral
                { Day16.litVersion = 6
                , Day16.litTypeID = 4
                , Day16.litValue = 10 }
            sub2 = Day16.PacketL $ Day16.MkLiteral
                { Day16.litVersion = 2
                , Day16.litTypeID = 4
                , Day16.litValue = 20 }
        in  (Day16.bitsToInt <<$>> Day16.getPacket hex2) @?= Right packet ]

sumVersionTests :: TestTree
sumVersionTests = Tasty.testGroup "sumOfVersions"
    [ HUnit.testCase "packet1" $ Day16.sumOfVersions packet1 @?= 16
    , HUnit.testCase "packet2" $ Day16.sumOfVersions packet2 @?= 12
    , HUnit.testCase "packet3" $ Day16.sumOfVersions packet3 @?= 23
    , HUnit.testCase "packet4" $ Day16.sumOfVersions packet4 @?= 31 ]

hex1 :: Text
hex1 = "D2FE28"

hex2 :: Text
hex2 = "38006F45291200"

packet1 :: Packet Int
packet1 = $$(
    liftTyped
        $ fmap Day16.bitsToInt
        $ fromRight undefined
        $ Day16.getPacket "8A004A801A8002F478" )

packet2 :: Packet Int
packet2 = $$(
    liftTyped
        $ fmap Day16.bitsToInt
        $ fromRight undefined
        $ Day16.getPacket "620080001611562C8802118E34" )

packet3 :: Packet Int
packet3 = $$(
    liftTyped
        $ fmap Day16.bitsToInt
        $ fromRight undefined
        $ Day16.getPacket "C0015000016115A2E0802F182340" )

packet4 :: Packet Int
packet4 = $$(
    liftTyped
        $ fmap Day16.bitsToInt
        $ fromRight undefined
        $ Day16.getPacket "A0016C880162017C3686B18A3D4780" )
