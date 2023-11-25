{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}

module Day10 where

import Control.Applicative.Combinators.NonEmpty qualified as Par.NE
import Control.Lens ((&), (+~), view)
import Control.Monad (void)
import Data.Foldable qualified as Fold
import Data.Generics.Labels ()  -- for #lens orphan instance
import Data.List.NonEmpty (NonEmpty)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Flow ((.>))
import GHC.Generics (Generic)
import Linear (V2(V2), _x, _y)
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch
import Text.Megaparsec.Char.Lexer qualified as Par.Ch.Lex

import Common


data Point a = MkPoint
    { position :: V2 a
    , velocity :: V2 a
    } deriving (Eq, Generic, Ord, Read, Show)

parseV2 :: Num a => Parser (V2 a)
parseV2 = do
    void $ Par.Ch.char '<' <* Par.Ch.hspace
    x <- decimal <* Par.Ch.hspace
    void $ Par.Ch.char ',' <* Par.Ch.hspace
    y <- decimal <* Par.Ch.hspace
    void $ Par.Ch.char '>' <* Par.Ch.hspace
    pure $ V2 x y
  where
    decimal = Par.Ch.Lex.signed Par.Ch.hspace Par.Ch.Lex.decimal

parsePoint :: Num a => Parser (Point a)
parsePoint = do
    void $ Par.Ch.string' "position" <* Par.Ch.hspace
    void $ Par.Ch.char '=' <* Par.Ch.hspace
    position <- parseV2
    void $ Par.Ch.string' "velocity" <* Par.Ch.hspace
    void $ Par.Ch.char '=' <* Par.Ch.hspace
    velocity <- parseV2
    pure $ MkPoint{..}

parsePoints :: Num a => Parser (NonEmpty (Point a))
parsePoints = parsePoint `Par.NE.sepEndBy1` Par.Ch.newline

getInput :: Num a => Text -> Either String (NonEmpty (Point a))
getInput = runParser (parsePoints <* Par.eof) "day-10"

step :: Num a => Point a -> Point a
step point = point & #position +~ velocity point

stepWhileDiamDecr
    :: (Num a, Ord a) => NonEmpty (Point a) -> (NonEmpty (Point a), Int)
stepWhileDiamDecr = go 0
  where
    go n points =
        let before = diameter (position <$> points)
            after = diameter (position <$> next)
            next = step <$> points
        in  if after <= before then go (n + 1) next else (points, n)

vizualize :: (Ord a, Enum a) => NonEmpty (V2 a) -> Text
vizualize points = Text.intercalate "\n" $ do
    y <- [infimumNE ys .. supremumNE ys]
    pure $ Text.pack $ do
        x <- [infimumNE xs .. supremumNE xs]
        pure $ if V2 x y `Set.member` set then '#' else '.'
  where
    xs = view _x <$> points
    ys = view _y <$> points
    set = Set.fromList $ Fold.toList points

distance :: Num a => V2 a -> V2 a -> a
distance u v = sum $ abs (v - u)

diameter :: (Num a, Ord a) => NonEmpty (V2 a) -> a
diameter points = distance inf sup
  where
    xs = view _x <$> points
    ys = view _y <$> points
    inf = V2 (infimumNE xs) (infimumNE ys)
    sup = V2 (supremumNE xs) (supremumNE ys)

part1 :: (Num a, Ord a, Enum a) => NonEmpty (Point a) -> Text
part1 = stepWhileDiamDecr .> fst .> fmap position .> vizualize

part2 :: (Num a, Ord a) => NonEmpty (Point a) -> Int
part2 = stepWhileDiamDecr .> snd

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-10.txt"
    case getInput @Int text of
        Left err -> die err
        Right input -> do
            Text.IO.putStrLn $ part1 input
            print $ part2 input
