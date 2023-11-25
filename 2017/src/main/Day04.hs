module Day04 where

import Common
import Data.Containers.ListUtils (nubOrd, nubOrdOn)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.List qualified as List
import Flow ((.>))


-- | Here we are depending on the not-explicitly-stated property of 'nubOrd'
-- that it is stable - that it preserves the original ordering of the list.
distinct :: Ord a => [a] -> Bool
distinct xs = xs == nubOrd xs

valid :: Text -> Bool
valid = Text.words .> distinct

part1 :: [Text] -> Int
part1 = filter valid .> length

noAnagrams :: Ord a => [[a]] -> Bool
noAnagrams xs = xs == nubOrdOn List.sort xs

valid2 :: Text -> Bool
valid2 = Text.words .> fmap Text.unpack .> noAnagrams

part2 :: [Text] -> Int
part2 = filter valid2 .> length

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-04.txt"
    let phrases = Text.lines text
    print $ part1 phrases
    print $ part2 phrases
