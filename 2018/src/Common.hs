{-# LANGUAGE LambdaCase #-}

module Common where

import Control.Monad ((>=>), guard)
import Data.Bifunctor (first)
import Data.ByteString qualified as Byt
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Enc
import Data.Vector qualified as Vec
import Data.Void (Void)
import Flow ((.>))
import Numeric.Interval (Interval)
import Numeric.Interval qualified as Inter
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as Par

import Paths_adventofcode2018 (getDataFileName)


readFileUtf8 :: FilePath -> IO Text
readFileUtf8 path = Text.Enc.decodeUtf8 <$> Byt.readFile path

readInputFileUtf8 :: FilePath -> IO Text
readInputFileUtf8 = getDataFileName >=> readFileUtf8

textShow :: Show a => a -> Text
textShow = show .> Text.pack

type Parser a = Parsec Void Text a

runParser :: Parser a -> String -> Text -> Either String a
runParser parser desc = Par.parse parser desc .> first Par.errorBundlePretty


between :: Ord a => a -> a -> a -> Bool
between a b x = (a <= x) && (x <= b)

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither def = \case
    Nothing -> Left def
    Just x  -> Right x

distinctUnorderedPairs :: [a] -> [(a, a)]
distinctUnorderedPairs list = do
    i <- [0 .. n - 1]
    j <- [0 .. i - 1]
    pure (vec Vec.! i, vec Vec.! j)
  where
    n = length vec
    vec = Vec.fromList list

supremum :: (Foldable t, Ord a) => a -> t a -> a
supremum = supremumBy compare

supremumBy :: Foldable t => (a -> a -> Ordering) -> a -> t a -> a
supremumBy cmp = foldl $
    \x y -> case x `cmp` y of
        GT -> x
        _  -> y

infimum :: (Foldable t, Ord a) => a -> t a -> a
infimum = infimumBy compare

infimumBy :: Foldable t => (a -> a -> Ordering) -> a -> t a -> a
infimumBy cmp = foldl $
    \x y -> case x `cmp` y of
        LT -> x
        _  -> y

maxEntriesByVal :: Ord a => Map k a -> [(k, a)]
maxEntriesByVal = Map.toList .> \case
    []         -> []
    xs@(x : _) ->
        let sup = supremum (snd x) (snd <$> xs)
        in  do  (key, val) <- xs
                guard $ val == sup
                pure (key, val)


mergeOverlaps :: Ord a => [Interval a] -> [Interval a]
mergeOverlaps = groupChains overlaps .> fmap mconcat
  where
    overlaps x y = not $ Inter.null $ Inter.intersection x y

groupChains :: (a -> a -> Bool) -> [a] -> [[a]]
groupChains f = go []
  where
    go chains = \case
        []     -> chains
        x : xs -> case tryInserts f x chains of
            Nothing  -> go ([x] : chains) xs
            Just new -> go new xs

tryInserts :: (a -> a -> Bool) -> a -> [[a]] -> Maybe [[a]]
tryInserts f x = \case
    []       -> Nothing
    xs : xss -> case tryInsert f x xs of
        Just ys -> Just (ys : xss)
        Nothing -> (xs :) <$> tryInserts f x xss

tryInsert :: (a -> a -> Bool) -> a -> [a] -> Maybe [a]
tryInsert f x xs = if any (f x) xs
    then Just (x : xs)
    else Nothing
