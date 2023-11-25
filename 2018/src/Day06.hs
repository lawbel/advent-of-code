{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}

module Day06 where

import Control.Applicative (empty)
import Control.Applicative.Combinators.NonEmpty qualified as Par.NE
import Control.Lens ((&), (^.))
import Control.Monad (void, guard)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List.NonEmpty qualified as List.NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text qualified as Text
import Flow ((.>), (<.))
import Linear (V2(V2), _x, _y)
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch
import Text.Megaparsec.Char.Lexer qualified as Par.Ch.Lex

import Common


data Box a = MkBox
    { minimal :: V2 a
    , maximal :: V2 a
    } deriving (Eq, Ord, Read, Show)

parseCoord :: Num a => Parser (V2 a)
parseCoord = do
    x <- Par.Ch.Lex.decimal <* Par.Ch.hspace
    void $ Par.Ch.char ',' <* Par.Ch.hspace
    y <- Par.Ch.Lex.decimal <* Par.Ch.hspace
    pure $ V2 x y

parseCoords :: Num a => Parser (NonEmpty (V2 a))
parseCoords = parseCoord `Par.NE.sepEndBy1` Par.Ch.newline

getInput :: Num a => Text -> Either String (NonEmpty (V2 a))
getInput = runParser (parseCoords <* Par.eof) "day-06"

dup :: a -> (a, a)
dup x = (x, x)

manhattan :: Num a => V2 a -> V2 a -> a
manhattan u v = sum $ abs (u - v)

sumOfManhattan :: (Num a, Foldable f, Functor f) => V2 a -> f (V2 a) -> a
sumOfManhattan u vs = sum (manhattan u <$> vs)

boxEnclosing :: Ord a => NonEmpty (V2 a) -> Box a
boxEnclosing (vec :| vecs) = MkBox
    { minimal = V2 (infimum x xs) (infimum y ys)
    , maximal = V2 (supremum x xs) (supremum y ys)
    }
  where
    x :| xs = (vec ^. _x) :| fmap (^. _x) vecs
    y :| ys = (vec ^. _y) :| fmap (^. _y) vecs

onBoundary :: Ord a => Box a -> V2 a -> Bool
onBoundary box (V2 x y) =
    (elem x [xMin, xMax] && between yMin yMax y) ||
    (elem y [yMin, yMax] && between xMin xMax x)
  where
    xMin = minimal box ^. _x
    yMin = minimal box ^. _y
    xMax = maximal box ^. _x
    yMax = maximal box ^. _y

closest :: (Num a, Ord a) => Map (V2 a) b -> V2 a -> [(V2 a, b)]
closest assoc v = do
    (u, tag) <- Map.toList assoc
    inf <- maybeToList maybeInf
    guard $ manhattan u v == inf
    pure (u, tag)
  where
    maybeInf = case manhattan v <$> Map.keys assoc of
        []     -> Nothing
        x : xs -> Just $ infimum x xs

closestWithin
    :: (Enum a, Num a, Ord a)
    => Box a -> Map (V2 a) b -> Map (V2 a) (a, NonEmpty b)
closestWithin box assoc = Map.fromList $ do
    x <- [inf ^. _x .. sup ^. _x]
    y <- [inf ^. _y .. sup ^. _y]
    let vec = V2 x y
    let locus = closest assoc vec
    (near, tag) <- take 1 locus
    let tags = snd <$> drop 1 locus
    let info = (manhattan vec near, tag :| tags)
    pure (vec, info)
  where
    inf = minimal box
    sup = maximal box

sumOfDistanceWithin
    :: (Enum a, Num a, Ord a)
    => Box a -> Map (V2 a) b -> Map (V2 a) (a, Maybe b)
sumOfDistanceWithin box assocs = Map.fromList $ do
    x <- [inf ^. _x .. sup ^. _x]
    y <- [inf ^. _y .. sup ^. _y]
    let vec = V2 x y
    let val = (sumOfManhattan vec vecs, assocs Map.!? vec)
    pure (vec, val)
  where
    vecs = Map.keys assocs
    inf = minimal box
    sup = maximal box

vizualizeWithin
    :: (Enum a, Ord a) => Box a -> Map (V2 a) b -> (Maybe b -> Char) -> Text
vizualizeWithin box assoc vizualize = Text.intercalate "\n" $ do
    y <- [inf ^. _y .. sup ^. _y]
    pure $ Text.pack $ do
        x <- [inf ^. _x .. sup ^. _x]
        pure $ vizualize $ Map.lookup (V2 x y) assoc
  where
    inf = minimal box
    sup = maximal box

countWithin :: (Enum a, Ord a, Ord b) => Box a -> Map (V2 a) b -> Map b Int
countWithin box assoc = Map.fromListWith (+) $ do
    x <- [inf ^. _x .. sup ^. _x]
    y <- [inf ^. _y .. sup ^. _y]
    case assoc Map.!? V2 x y of
        Nothing  -> empty
        Just tag -> pure (tag, 1)
  where
    inf = minimal box
    sup = maximal box

finiteAreasInBoxEnclosing
    :: (Enum a, Num a, Ord a) => NonEmpty (V2 a) -> Map (V2 a) Int
finiteAreasInBoxEnclosing vecs =
    List.NE.toList vecs
        & fmap dup
        & Map.fromList
        & closestWithin box
        & Map.mapMaybe getTag
        & countWithin box
        & Map.filterWithKey (\v _ -> v `elem` interior)
  where
    box = boxEnclosing vecs
    interior = List.NE.filter (not <. onBoundary box) vecs
    getTag = snd .> \case
        v :| [] -> Just v
        _       -> Nothing

regionInBoxEnclosing
    :: (Enum a, Num a, Ord a) => a -> NonEmpty (V2 a) -> Map (V2 a) a
regionInBoxEnclosing level vecs =
    List.NE.toList vecs
        & fmap (, ())
        & Map.fromList
        & sumOfDistanceWithin (boxEnclosing vecs)
        & fmap fst
        & Map.filter (< level)

largestAreaInBoxEnclosing :: (Enum a, Num a, Ord a) => NonEmpty (V2 a) -> Int
largestAreaInBoxEnclosing = finiteAreasInBoxEnclosing .> supremum 0

regionSizeInBoxEnclosing
    :: (Enum a, Num a, Ord a) => a -> NonEmpty (V2 a) -> Int
regionSizeInBoxEnclosing level vecs =
    Map.size $ regionInBoxEnclosing level vecs

part1 :: (Enum a, Num a, Ord a) => NonEmpty (V2 a) -> Int
part1 = largestAreaInBoxEnclosing

part2 :: (Enum a, Num a, Ord a) => NonEmpty (V2 a) -> Int
part2 = regionSizeInBoxEnclosing 10_000

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-06.txt"
    case getInput @Int text of
        Left err -> die err
        Right input -> do
            print $ part1 input
            print $ part2 input
