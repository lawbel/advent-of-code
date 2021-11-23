{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day04 where

import Control.Applicative ((<|>))
import Control.Lens (makePrisms, preview)
import Control.Lens.Extras (is)
import Control.Monad (void, guard)
import Data.Fixed (Pico)
import Data.Foldable (asum)
import Data.Function ((&), on)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Data.Time (LocalTime, Day)
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 qualified as ISO
import Data.Tuple (swap)
import Flow ((.>))
import Numeric.Interval (Interval, (...))
import Numeric.Interval qualified as Inter
import System.Exit (die)
import Text.InterpolatedString.Perl6 (qc)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch
import Text.Megaparsec.Char.Lexer qualified as Par.Ch.Lex

import Common


newtype Guard = MkGuard
    { unGuard :: Int
    } deriving (Eq, Ord, Read, Show)

data Wake
    = FallAsleep
    | WakeUp
    deriving (Eq, Ord, Read, Show)

makePrisms ''Wake

data Event
    = Sleep Wake
    | Shift Guard
    deriving (Eq, Ord, Read, Show)

makePrisms ''Event

data RecordOf a = MkRecordOf
    { dateTime :: LocalTime
    , event :: a
    } deriving (Eq, Ord, Read, Show)


parseEvent :: Parser Event
parseEvent = asum
    [ Shift <$> parseGuardShift
    , Sleep <$> parseFallAsleep
    , Sleep <$> parseWakeUp
    ]

parseGuardShift :: Parser Guard
parseGuardShift = do
    void $ Par.Ch.string' "Guard" <* Par.Ch.hspace
    num <- Par.Ch.char '#' *> Par.Ch.Lex.decimal <* Par.Ch.hspace
    void $ Par.Ch.string' "begins" <* Par.Ch.hspace
    void $ Par.Ch.string' "shift" <* Par.Ch.hspace
    pure $ MkGuard num

parseFallAsleep :: Parser Wake
parseFallAsleep = FallAsleep <$ do
    void $ Par.Ch.string' "falls" <* Par.Ch.hspace
    void $ Par.Ch.string' "asleep" <* Par.Ch.hspace

parseWakeUp :: Parser Wake
parseWakeUp = WakeUp <$ do
    void $ Par.Ch.string' "wakes" <* Par.Ch.hspace
    void $ Par.Ch.string' "up" <* Par.Ch.hspace

parseDateTime :: Parser LocalTime
parseDateTime = do
    void $ Par.Ch.char '[' <* Par.Ch.hspace
    year <- Par.Ch.Lex.decimal <* Par.Ch.char '-'
    month <- Par.Ch.Lex.decimal <* Par.Ch.char '-'
    day <- Par.Ch.Lex.decimal <* Par.Ch.hspace
    hour <- Par.Ch.Lex.decimal <* Par.Ch.char ':'
    minute <- Par.Ch.Lex.decimal <* Par.Ch.hspace
    void $ Par.Ch.char ']' <* Par.Ch.hspace
    let date = Time.fromGregorian year month day
    pure $ makeLocalTime date (hour, minute, 0)

parseRecord :: Parser (RecordOf Event)
parseRecord = do
    dateTime <- parseDateTime
    event <- parseEvent
    pure $ MkRecordOf{..}

parseRecords :: Parser [RecordOf Event]
parseRecords = parseRecord `Par.sepEndBy` Par.Ch.newline

getInput :: Text -> Either String [RecordOf Event]
getInput = runParser (parseRecords <* Par.eof) "day-04"


prettyEvent :: Event -> Text
prettyEvent = \case
    Shift (MkGuard n) -> [qc|Guard #{n} begins shift|]
    Sleep FallAsleep  -> "falls asleep"
    Sleep WakeUp      -> "wakes up"

prettyRecord :: RecordOf Event -> Text
prettyRecord MkRecordOf{..} = [qc|[{date} {timeOfDay}] {prettyEvent event}|]
  where
    timeOfDay = ISO.formatShow
        (ISO.hourMinuteFormat ISO.ExtendedFormat)
        (Time.localTimeOfDay dateTime)
    date = ISO.formatShow ISO.iso8601Format (Time.localDay dateTime)

makeLocalTime :: Day -> (Int, Int, Pico) -> LocalTime
makeLocalTime date (hour, minute, second) = Time.LocalTime
    { Time.localDay = date
    , Time.localTimeOfDay = Time.TimeOfDay
        { Time.todHour = hour
        , Time.todMin = minute
        , Time.todSec = second
        }
    }

makeRecord :: (Integer, Int, Int) -> (Int, Int) -> a -> RecordOf a
makeRecord (year, month, day) (hour, minute) event = MkRecordOf
    { dateTime = makeLocalTime
        (Time.fromGregorian year month day)
        (hour, minute, 0)
    , event = event
    }

visualizeRecordsInHour :: Int -> Map (Day, Guard) [RecordOf Wake] -> Text
visualizeRecordsInHour hour records = header <> rows
  where
    rows = Text.intercalate "\n" $ do
        (key, sleeps) <- Map.toAscList records
        pure $ uncurry visualizeRow key sleeps
    idWidth = Map.keys records
        & fmap (snd .> unGuard .> textShow .> Text.length)
        & supremum 0
    visualizeRow date num sleeps =
        let sleeping = asleepDuring sleeps
            minutes = Text.pack $ do
                minute <- [0 .. 59]
                let time = makeLocalTime date (hour, minute, 30)
                pure $ if any (time `Inter.member`) sleeping then '#' else '.'
            numJustL = Text.justifyLeft idWidth ' ' $ textShow $ unGuard num
        in  [qc|{date}  #{numJustL}  {minutes}|]
    header = Text.unlines
        [ [qc|Date        {idHead}  Minute|]
        , [qc|            {idGaps}  {tens}|]
        , [qc|            {idGaps}  {ones}|]
        ]
    idHead = Text.justifyLeft (idWidth + 1) ' ' "ID"
    idGaps = Text.pack $ List.replicate (idWidth + 1) ' '
    tens = [0 .. 5] & fmap (textShow @Int .> Text.replicate 10) & mconcat
    ones = [0 .. 9] & fmap (textShow @Int) & mconcat & Text.replicate 6


asleepDuring :: [RecordOf Wake] -> [Interval LocalTime]
asleepDuring records = do
    start <- starts
    stop <- maybeToList $ case filter (> start) stops of
        []     -> Nothing
        x : xs -> Just $ infimum x xs
    pure (start ... stop)
  where
    starts = records & filter (event .> is _FallAsleep) & fmap dateTime
    stops = records & filter (event .> is _WakeUp) & fmap dateTime

seconds :: Interval LocalTime -> Int
seconds inter
    | Inter.null inter = 0
    | otherwise =
        let diff = Inter.sup inter `Time.diffLocalTime` Inter.inf inter
        in  round $ Time.nominalDiffTimeToSeconds diff

countMinutesIn :: Interval LocalTime -> Map Int Int
countMinutesIn inter
    | Inter.null inter = Map.empty
    | otherwise = Map.fromListWith (+) $ do
        let dayInf = Time.localDay inf
        let daySup = Time.localDay sup
        day <- [dayInf .. daySup]
        let hourInf = if day > dayInf then 00 else getHour inf
        let hourSup = if day < daySup then 23 else getHour sup
        hour <- [hourInf .. hourSup]
        let minInf = if hour > hourInf then 00 else getMin inf
        let minSup = if hour < hourSup then 59 else getMin sup
        minute <- [minInf .. minSup]
        let time = makeLocalTime day (hour, minute, 30)
        guard $ time `Inter.member` inter
        pure (minute, 1)
  where
    sup = Inter.sup inter
    inf = Inter.inf inter
    getHour = Time.localTimeOfDay .> Time.todHour
    getMin = Time.localTimeOfDay .> Time.todMin


tryProcessGroup
    :: [RecordOf Event]
    -> Either String ((Day, Guard), [RecordOf Wake])
tryProcessGroup group = do
    date <- maybeToEither "no date" $ tryGetSleep group <|> tryGetDate group
    num <- maybeToEither "no ID" $ tryGetId group
    let key = (date, num)
    pure (key, mapMaybe asSleepRecord group)
  where
    asSleepRecord MkRecordOf{..} = case event of
        Sleep sl -> Just $ MkRecordOf{event = sl, ..}
        Shift _  -> Nothing
    tryGetSleep = filter (event .> is _Sleep) .> tryGetDate
    tryGetDate = fmap (dateTime .> Time.localDay) .> listToMaybe
    tryGetId = mapMaybe (event .> preview _Shift) .> listToMaybe

tryGroupRecords
    :: [RecordOf Event]
    -> Either String (Map (Day, Guard) [RecordOf Wake])
tryGroupRecords records = do
    let sorted = List.sortOn dateTime records
    let grouped = List.groupBy ((/=) `on` isShift) sorted
    Map.fromList <$> traverse tryProcessGroup grouped
  where
    isShift = event .> is _Shift

countTimeAsleep :: Map (Day, Guard) [RecordOf Wake] -> Map Guard Int
countTimeAsleep records = Map.fromListWith (+) $ do
    (key, sleeping) <- Map.toList records
    let sleeps = mergeOverlaps $ asleepDuring sleeping
    let minutes = sum $ fmap getMins sleeps
    pure (snd key, minutes)
  where
    getMins = seconds .> fromIntegral .> (/ 60) .> round @Double

mostTimeAsleep :: Map (Day, Guard) [RecordOf Wake] -> [Guard]
mostTimeAsleep = countTimeAsleep .> maxEntriesByVal .> fmap fst

countMinutes :: Map (Day, Guard) [RecordOf Wake] -> Map (Int, Guard) Int
countMinutes records = Map.unionsWith (+) $ do
    num <- guards
    pure $ Map.mapKeys (, num) $ countMinutesFor num records
  where
    guards = snd <$> Map.keys records

countMinutesFor :: Guard -> Map (Day, Guard) [RecordOf Wake] -> Map Int Int
countMinutesFor num records = Map.fromList $ maxEntriesByVal minutes
  where
    matches (key, sleeps) = if snd key == num then Just sleeps else Nothing
    intervals = concatMap asleepDuring $ mapMaybe matches $ Map.toList records
    minutes = Map.unionsWith (+) $ fmap countMinutesIn intervals

mostFreqAsleep :: Map (Day, Guard) [RecordOf Wake] -> [(Int, Guard)]
mostFreqAsleep records = do
    supCount <- maybeToList maybeSup
    (key, count) <- counter
    guard $ count == supCount
    pure key
  where
    counter = Map.toList $ countMinutes records
    maybeSup = case snd <$> counter of
        []     -> Nothing
        x : xs -> Just $ supremum x xs

sleepiestMinutesFor :: Guard -> Map (Day, Guard) [RecordOf Wake] -> [Int]
sleepiestMinutesFor num = countMinutesFor num .> Map.keys

strategy1 :: [RecordOf Event] -> Either String (Guard, Int)
strategy1 records = do
    grouped <- tryGroupRecords records
    num <- case mostTimeAsleep grouped of
        [num] -> pure num
        nums  -> Left
            [qc|no unique guard most time slept: {unGuard <$> nums}|]
    minute <- case sleepiestMinutesFor num grouped of
        [minute] -> pure minute
        minutes  -> Left
            [qc|no unique minute most asleep for #{unGuard num}: {minutes}|]
    pure (num, minute)

strategy2 :: [RecordOf Event] -> Either String (Guard, Int)
strategy2 records = do
    grouped <- tryGroupRecords records
    case mostFreqAsleep grouped of
        [tup] -> pure $ swap tup
        freq  -> Left [qc|no unique minute most frequently asleep: {freq}|]

part1 :: [RecordOf Event] -> Either String Int
part1 = strategy1 .> \case
    Left err            -> Left err
    Right (num, minute) -> pure $ unGuard num * minute

part2 :: [RecordOf Event] -> Either String Int
part2 = strategy2 .> \case
    Left err            -> Left err
    Right (num, minute) -> pure $ unGuard num * minute

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-04.txt"
    case getInput text of
        Left err    -> die err
        Right input -> do
            -- case tryGroupRecords input of
            --     Left err      -> die err
            --     Right records ->
            --         Text.IO.putStrLn $ visualizeRecordsInHour 0 records
            print $ part1 input
            print $ part2 input
