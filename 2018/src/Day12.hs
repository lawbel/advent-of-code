{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day12 where

import Control.Applicative (many)
import Control.Lens ((&), (%~), _1, _Just)
import Data.Char qualified as Char
import Data.Foldable (asum)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Flow ((.>))
import Linear (V2(V2))
import System.Exit (die)
import Text.InterpolatedString.Perl6 (qq)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch

import Common


data Plant = Dead | Alive
    deriving (Eq, Ord, Read, Show)

data Match a = MkMatch
    { lefts  :: V2 a
    , center :: a
    , rights :: V2 a
    } deriving (Eq, Ord, Read, Show)

parsePlant :: Parser Plant
parsePlant = asum
    [ Dead  <$ Par.Ch.char '.'
    , Alive <$ Par.Ch.char '#'
    ]

parseInitState :: Parser [Plant]
parseInitState = do
    Par.Ch.string' "initial" *> Par.Ch.hspace
    Par.Ch.string' "state:"  *> Par.Ch.hspace
    many parsePlant <* Par.Ch.hspace

parseRule :: Parser (Match Plant, Plant)
parseRule = do
    lefts  <- V2 <$> parsePlant <*> parsePlant
    center <- parsePlant
    rights <- V2 <$> parsePlant <*> parsePlant
    Par.Ch.hspace *> Par.Ch.string "=>" *> Par.Ch.hspace
    result <- parsePlant <* Par.Ch.hspace
    pure (MkMatch{..}, result)

parseRules :: Parser (Map (Match Plant) Plant)
parseRules = Map.fromList <$> Par.sepEndBy parseRule Par.Ch.newline

parseInput :: Parser ([Plant], Map (Match Plant) Plant)
parseInput = do
    initState <- parseInitState <* Par.Ch.space
    rules <- parseRules <* Par.Ch.space
    pure (initState, rules)

getInput :: Text -> Either String ([Plant], Map (Match Plant) Plant)
getInput = runParser (parseInput <* Par.eof) "day-12"

prettyPlant :: Plant -> Char
prettyPlant = \case
    Dead  -> '.'
    Alive -> '#'

prettyPlants :: Plant -> IntMap Plant -> (Text, Int)
prettyPlants def tape = (, l) $ Text.pack $ do
    i <- [l .. r]
    pure $ prettyPlant $ IntMap.findWithDefault def i tape
  where
    indices = IntMap.keysSet tape
    l = maybe 0 fst $ IntSet.minView indices
    r = maybe 0 fst $ IntSet.maxView indices

iter :: Int -> (a -> a) -> a -> a
iter n fn
    | n <= 0    = id
    | otherwise = fn .> iter (n - 1) fn

step :: Ord a => Map (Match a) a -> a -> IntMap a -> IntMap a
step rules def tape = nextTape
    & trimTape IntMap.minViewWithKey
    & trimTape IntMap.maxViewWithKey
  where
    getDef i = IntMap.findWithDefault def i tape
    wideTape =
        let indices = IntMap.keysSet tape
            l = maybe 0 fst $ IntSet.minView indices
            r = maybe 0 fst $ IntSet.maxView indices
            edge = IntMap.fromList $ fmap (, def) [l - 2, l - 1, r + 1, r + 2]
        in  tape <> edge
    nextTape = IntMap.fromList $ do
        (i, center) <- IntMap.toList wideTape
        let lefts  = getDef <$> V2 (i - 2) (i - 1)
        let rights = getDef <$> V2 (i + 1) (i + 2)
        let match  = MkMatch{..}
        pure (i, Map.findWithDefault def match rules)
    trimTape view assoc = case view assoc & _Just . _1 %~ snd of
        Just (x, next) | x == def -> trimTape view next
        _                         -> assoc

prettySteps :: Int -> Map (Match Plant) Plant -> Plant -> IntMap Plant -> Text
prettySteps n rules def tape = Text.intercalate "\n" [tensHead, onesHead, body]
  where
    defChar = prettyPlant def
    defText = Text.singleton defChar
    rows = do
        plants <- scanl (\plants _i -> step rules def plants) tape [1 .. n]
        pure $ prettyPlants def plants
    inf = infimum 0 $ fmap snd rows
    rowsL = do
        (text, i) <- rows
        pure $ Text.replicate (i - inf) defText <> text
    sup = supremum 0 $ fmap Text.length rowsL
    ix = Text.length $ textShow $ length rowsL
    body = Text.intercalate "\n" $ do
        (i, row) <- zip [0..] rowsL
        let text = mconcat [defText, Text.justifyLeft sup defChar row, defText]
        let num = Text.justifyRight ix ' ' $ textShow @Int i
        pure [qq|$num: $text|]
    preHead = Text.replicate (ix + 3) " "
    onesHead = flip mappend " " $ mappend preHead $ Text.pack $ do
        i <- [inf .. sup + inf - 1]
        pure $ if i `mod` 10 == 0 then '0' else ' '
    tensHead = flip mappend " " $ mappend preHead $ Text.pack $ do
        i <- [inf .. sup + inf - 1]
        pure $ if
            | i == 0          -> ' '
            | i `mod` 10 == 0 -> tensDigit i
            | otherwise       -> ' '
    tensDigit i = Char.chr $ Char.ord '0' + (i `div` 10 `mod` 10)

asIntMap :: [a] -> IntMap a
asIntMap = zip [0..] .> IntMap.fromList

sumIndexAliveAfter :: Int -> [Plant] -> Map (Match Plant) Plant -> Int
sumIndexAliveAfter n plants rules =
    iter n (step rules Dead) (asIntMap plants)
        & IntMap.filter (== Alive)
        & IntMap.keys
        & sum

-- | If the result of one 'step' is the same as the previous (except for being
-- shifted over left/right by some amount), then the result of subsequent calls
-- to 'step' will have the same effect of merely shifting over the plants.
--
-- If this is not readily apparent, consider that the rules for how plants
-- develop are independent of the plants position.
--
-- Given this fact, our strategy for handling very large numbers of steps is:
--   * identify when we enter a loop as just desribed, and stop iterating at
--     that point,
--   * calculate how much we /would have drifted/ if we completed the
--     remaining steps,
--   * add the right amount onto our final sum.
sumIndexAliveAfter' :: Int -> [Plant] -> Map (Match Plant) Plant -> Integer
sumIndexAliveAfter' m plants rules = sum $ fmap (+ drift) tape
  where
    (tape, drift) = go m (asIntMap plants) & _1 %~ aliveKeys
    go i cur =
        let next = step rules Dead cur
        in  if  | i <= 0 -> (cur, 0)
                | Just c <- getMinKey cur
                , Just n <- getMinKey next
                , IntMap.mapKeys (+ c) next == IntMap.mapKeys (+ n) cur ->
                    ( cur
                    , fromIntegral $ i * (n - c)
                    )
                | otherwise -> go (i - 1) next
    aliveKeys = IntMap.filter (== Alive) .> IntMap.keys .> fmap fromIntegral
    getMinKey = IntMap.minViewWithKey .> fmap (fst .> fst)

part1 :: [Plant] -> Map (Match Plant) Plant -> Int
part1 = sumIndexAliveAfter 20

part2 :: [Plant] -> Map (Match Plant) Plant -> Integer
part2 = sumIndexAliveAfter' 50_000_000_000

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-12.txt"
    case getInput text of
        Left err -> die err
        Right (plants, rules) -> do
            print $ part1 plants rules
            print $ part2 plants rules
