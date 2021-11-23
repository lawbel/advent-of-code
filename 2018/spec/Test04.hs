{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test04 where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (LocalTime, Day)
import Data.Time qualified as Time
import Data.Time.Calendar.OrdinalDate qualified as Time.Cal
import Flow ((.>))
import Test.Tasty (TestTree)
import Test.Tasty qualified as Tasty
import Test.Tasty.HUnit ((@?=))
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck (Arbitrary)
import Test.Tasty.QuickCheck qualified as Check
import Text.Megaparsec qualified as Par

import Day04 (RecordOf, Event, Guard, Wake)
import Day04 qualified


instance Arbitrary Event where
    arbitrary = Check.oneof
        [ beginShift
        , pure $ Day04.Sleep Day04.FallAsleep
        , pure $ Day04.Sleep Day04.WakeUp
        ]
      where
        beginShift = do
            Check.Positive num <- Check.arbitrary
            pure $ Day04.Shift $ Day04.MkGuard num

instance Arbitrary LocalTime where
    arbitrary = do
        year <- Check.chooseInteger (1000, 3000)
        dayOfYear <- Check.chooseInt (1, 366)
        hour <- Check.chooseInt (0, 23)
        minute <- Check.chooseInt (0, 59)
        pure $ Time.LocalTime
            { Time.localDay = Time.Cal.fromOrdinalDate year dayOfYear
            , Time.localTimeOfDay = Time.TimeOfDay
                { Time.todHour = hour
                , Time.todMin = minute
                , Time.todSec = 0
                }
            }

instance Arbitrary (RecordOf Event) where
    arbitrary = do
        event <- Check.arbitrary
        dateTime <- Check.arbitrary
        pure $ Day04.MkRecordOf{..}

main :: IO ()
main = Tasty.defaultMain tests

tests :: TestTree
tests = Tasty.testGroup "tests"
    [ parseTests
    , prettyTests
    , unitTests
    , propertyTests
    ]

propertyTests :: TestTree
propertyTests = Tasty.testGroup "properties"
    [ Check.testProperty "prettyRecord .> parseRecord == Just" $ \record ->
        let roundTrip = Day04.prettyRecord .> Par.parseMaybe Day04.parseRecord
        in  roundTrip record == Just record
    ]

recordEx1 :: RecordOf Event
recordEx1 = Day04.makeRecord (1518, 11, 01) (00, 00) $
    Day04.Shift $ Day04.MkGuard 10

recordEx2 :: RecordOf Event
recordEx2 = Day04.makeRecord (1518, 11, 01) (00, 05) $
    Day04.Sleep Day04.FallAsleep

recordEx3 :: RecordOf Event
recordEx3 = Day04.makeRecord (1518, 11, 01) (00, 25) $
    Day04.Sleep Day04.WakeUp

parseTests :: TestTree
parseTests = Tasty.testGroup "parse tests"
    [ HUnit.testCase "example 1" $
        let str = "[1518-11-01 00:00] Guard #10 begins shift"
        in  Par.parseMaybe Day04.parseRecord str @?= Just recordEx1
    , HUnit.testCase "example 2" $
        let str = "[1518-11-01 00:05] falls asleep"
        in  Par.parseMaybe Day04.parseRecord str @?= Just recordEx2
    , HUnit.testCase "example 3" $
        let str = "[1518-11-01 00:25] wakes up"
        in  Par.parseMaybe Day04.parseRecord str @?= Just recordEx3
   ]

prettyTests :: TestTree
prettyTests = Tasty.testGroup "pretty tests"
    [ HUnit.testCase "example 1" $
        let str = "[1518-11-01 00:00] Guard #10 begins shift"
        in  Day04.prettyRecord recordEx1 @?= str
    , HUnit.testCase "example 2" $
        Day04.prettyRecord recordEx2 @?= "[1518-11-01 00:05] falls asleep"
    , HUnit.testCase "example 3" $
        Day04.prettyRecord recordEx3 @?= "[1518-11-01 00:25] wakes up"
    ]

unitTests :: TestTree
unitTests = Tasty.testGroup "unit tests"
    [ HUnit.testCase "tryGroupRecords" $
        Day04.tryGroupRecords examples @?= Right groups
    , HUnit.testCase "visualizeRecordsInHour" $
        Day04.visualizeRecordsInHour 0 groups @?= table
    , HUnit.testCase "countTimeAsleep" $
        let time = Map.fromList
                [ (Day04.MkGuard 10, 50)
                , (Day04.MkGuard 99, 30)
                ]
        in  Day04.countTimeAsleep groups @?= time
    , HUnit.testCase "sleepiestMinutesFor" $
        Day04.sleepiestMinutesFor (Day04.MkGuard 10) groups @?= [24]
    , HUnit.testCase "strategy1" $
        Day04.strategy1 examples @?= Right (Day04.MkGuard 10, 24)
    , HUnit.testCase "strategy2" $
        Day04.strategy2 examples @?= Right (Day04.MkGuard 99, 45)
    ]

table :: Text
table = Text.intercalate "\n"
    [ "Date        ID   Minute"
    , "                 000000000011111111112222222222333333333344444444445555555555"
    , "                 012345678901234567890123456789012345678901234567890123456789"
    , "1518-11-01  #10  .....####################.....#########################....."
    , "1518-11-02  #99  ........................................##########.........."
    , "1518-11-03  #10  ........................#####..............................."
    , "1518-11-04  #99  ....................................##########.............."
    , "1518-11-05  #99  .............................................##########....."
    ]

examples :: [RecordOf Event]
examples =
    [ Day04.makeRecord (1518, 11, 01) (00, 00) $ makeShift 10
    , Day04.makeRecord (1518, 11, 01) (00, 05) $ Day04.Sleep Day04.FallAsleep
    , Day04.makeRecord (1518, 11, 01) (00, 25) $ Day04.Sleep Day04.WakeUp
    , Day04.makeRecord (1518, 11, 01) (00, 30) $ Day04.Sleep Day04.FallAsleep
    , Day04.makeRecord (1518, 11, 01) (00, 55) $ Day04.Sleep Day04.WakeUp
    , Day04.makeRecord (1518, 11, 01) (23, 58) $ makeShift 99
    , Day04.makeRecord (1518, 11, 02) (00, 40) $ Day04.Sleep Day04.FallAsleep
    , Day04.makeRecord (1518, 11, 02) (00, 50) $ Day04.Sleep Day04.WakeUp
    , Day04.makeRecord (1518, 11, 03) (00, 05) $ makeShift 10
    , Day04.makeRecord (1518, 11, 03) (00, 24) $ Day04.Sleep Day04.FallAsleep
    , Day04.makeRecord (1518, 11, 03) (00, 29) $ Day04.Sleep Day04.WakeUp
    , Day04.makeRecord (1518, 11, 04) (00, 02) $ makeShift 99
    , Day04.makeRecord (1518, 11, 04) (00, 36) $ Day04.Sleep Day04.FallAsleep
    , Day04.makeRecord (1518, 11, 04) (00, 46) $ Day04.Sleep Day04.WakeUp
    , Day04.makeRecord (1518, 11, 05) (00, 03) $ makeShift 99
    , Day04.makeRecord (1518, 11, 05) (00, 45) $ Day04.Sleep Day04.FallAsleep
    , Day04.makeRecord (1518, 11, 05) (00, 55) $ Day04.Sleep Day04.WakeUp
    ]
  where
    makeShift = Day04.MkGuard .> Day04.Shift

groups :: Map (Day, Guard) [RecordOf Wake]
groups = Map.fromList
    [ ( makeKey (1518, 11, 01) 10
      , [ Day04.makeRecord (1518, 11, 01) (00, 05) Day04.FallAsleep
        , Day04.makeRecord (1518, 11, 01) (00, 25) Day04.WakeUp
        , Day04.makeRecord (1518, 11, 01) (00, 30) Day04.FallAsleep
        , Day04.makeRecord (1518, 11, 01) (00, 55) Day04.WakeUp
        ]
      )
    , ( makeKey (1518, 11, 02) 99
      , [ Day04.makeRecord (1518, 11, 02) (00, 40) Day04.FallAsleep
        , Day04.makeRecord (1518, 11, 02) (00, 50) Day04.WakeUp
        ]
      )
    , ( makeKey (1518, 11, 03) 10
      , [ Day04.makeRecord (1518, 11, 03) (00, 24) Day04.FallAsleep
        , Day04.makeRecord (1518, 11, 03) (00, 29) Day04.WakeUp
        ]
      )
    , ( makeKey (1518, 11, 04) 99
      , [ Day04.makeRecord (1518, 11, 04) (00, 36) Day04.FallAsleep
        , Day04.makeRecord (1518, 11, 04) (00, 46) Day04.WakeUp
        ]
      )
    , ( makeKey (1518, 11, 05) 99
      , [ Day04.makeRecord (1518, 11, 05) (00, 45) Day04.FallAsleep
        , Day04.makeRecord (1518, 11, 05) (00, 55) Day04.WakeUp
        ]
      )
    ]
  where
    makeKey (year, month, day) num =
        ( Time.fromGregorian year month day
        , Day04.MkGuard num
        )
