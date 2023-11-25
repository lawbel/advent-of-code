{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Day03 where

import Control.Lens ((^.), view)
import Control.Monad (void, guard)
import Data.Containers.ListUtils (nubOrd)
import Data.List qualified as List
import Data.Text (Text)
import Flow ((.>))
import Linear (V2(V2), _x, _y)
import Numeric.Interval (Interval, (...))
import Numeric.Interval qualified as Inter
import System.Exit (die)
import Text.InterpolatedString.Perl6 (qc)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch
import Text.Megaparsec.Char.Lexer qualified as Par.Ch.Lex

import Common


data Claim a = MkClaim
    { idNum :: Int
    , base :: V2 a
    , size :: V2 a
    } deriving (Eq, Ord, Show, Read)

data Box a = MkBox
    { xAxis :: Interval a
    , yAxis :: Interval a
    } deriving (Eq, Ord, Show)

instance Ord a => Semigroup (Box a) where
    (<>) = boxHull

instance Ord a => Monoid (Box a) where
    mempty = boxEmpty

parseSepV2 :: Num a => Parser b -> Parser (V2 a)
parseSepV2 sep = do
    x <- Par.Ch.Lex.decimal <* Par.Ch.hspace
    void sep
    y <- Par.Ch.Lex.decimal <* Par.Ch.hspace
    pure $ V2 x y

parseClaim :: Num a => Parser (Claim a)
parseClaim = do
    void $ Par.Ch.char '#' <* Par.Ch.hspace
    idNum <- Par.Ch.Lex.decimal <* Par.Ch.hspace
    void $ Par.Ch.char '@' <* Par.Ch.hspace
    base <- parseSepV2 $ Par.Ch.char ',' <* Par.Ch.hspace
    void $ Par.Ch.char ':' <* Par.Ch.hspace
    size <- parseSepV2 $ Par.Ch.char 'x' <* Par.Ch.hspace
    pure $ MkClaim{..}

parseClaims :: Num a => Parser [Claim a]
parseClaims = parseClaim `Par.sepEndBy` Par.Ch.newline

getInput :: Num a => Text -> Either String [Claim a]
getInput = runParser (parseClaims <* Par.eof) "day-03"

prettyClaim :: Show a => Claim a -> Text
prettyClaim MkClaim{..} =
    [qc|#{idNum} @ {base ^. _x},{base ^. _y}: {size ^. _x}x{size ^. _y}|]


claimToBox :: (Ord a, Num a) => Claim a -> Box a
claimToBox MkClaim{..} = MkBox
    { xAxis = view _x base ... (view _x base + view _x size)
    , yAxis = view _y base ... (view _y base + view _y size)
    }

boxIntersect :: Ord a => Box a -> Box a -> Box a
boxIntersect one two = MkBox
    { xAxis = Inter.intersection (xAxis one) (xAxis two)
    , yAxis = Inter.intersection (yAxis one) (yAxis two)
    }

boxNull :: Box a -> Bool
boxNull MkBox{..} = Inter.null xAxis || Inter.null yAxis

boxEmpty :: Box a
boxEmpty = MkBox Inter.empty Inter.empty

boxHull :: Ord a => Box a -> Box a -> Box a
boxHull one two = MkBox
    { xAxis = Inter.hull (xAxis one) (xAxis two)
    , yAxis = Inter.hull (yAxis one) (yAxis two)
    }

boxArea :: Num a => Box a -> a
boxArea MkBox{..} = Inter.width xAxis * Inter.width yAxis

withinBox :: Ord a => V2 a -> Box a -> Bool
withinBox (V2 x y) MkBox{..} =
    (x `Inter.member` xAxis) && (y `Inter.member` yAxis)


conflicts :: (Num a, Ord a) => [Claim a] -> [Box a]
conflicts claims = do
    (one, two) <- distinctUnorderedPairs claims
    let box = claimToBox one `boxIntersect` claimToBox two
    guard $ boxArea box > 0
    pure box

groupConflicts :: Ord a => [Box a] -> [[Box a]]
groupConflicts = List.sort .> List.groupBy intersect
  where
    intersect x y = not $ boxNull $ boxIntersect x y

overlaps :: RealFrac a => [Box a] -> [V2 a]
overlaps boxes
    | boxNull hull = []
    | otherwise = do
        x <- [round (Inter.inf xs) .. round (Inter.sup xs) - 1]
        y <- [round (Inter.inf ys) .. round (Inter.sup ys) - 1]
        let midPoint = fmap (fromIntegral @Int) (V2 x y) + pure 0.5
        guard $ any (midPoint `withinBox`) boxes
        pure midPoint
  where
    xs = xAxis hull
    ys = yAxis hull
    hull = mconcat boxes

notOverlapped :: (Num a, Ord a) => [Claim a] -> [Claim a]
notOverlapped claims = do
    claim <- claims
    let noOverlap other =
            idNum other == idNum claim ||
            boxArea (claimToBox other `boxIntersect` claimToBox claim) == 0
    guard $ all noOverlap claims
    pure claim


part1 :: RealFrac a => [Claim a] -> Int
part1 = conflicts
    .> groupConflicts
    .> fmap overlaps
    .> concat
    .> nubOrd
    .> length

part2 :: (Num a, Ord a) => [Claim a] -> [Int]
part2 = notOverlapped .> fmap idNum

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-03.txt"
    case getInput @Float text of
        Left err -> die err
        Right input -> do
            print $ part1 input
            print $ part2 input
