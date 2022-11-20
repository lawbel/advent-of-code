module Day04 where

import Common
import Data.Containers.ListUtils (nubOrd)
import Data.Text (Text)
import Data.Text qualified as Text
import Flow ((.>))


-- | Here we are depending on the not-explicitly-stated property of 'nubOrd'
-- that it is stable - that it preserves the original ordering of the list.
distinct :: Ord a => [a] -> Bool
distinct xs = xs == nubOrd xs

valid :: Text -> Bool
valid = Text.words .> distinct

part1 :: [Text] -> Int
part1 = filter valid .> length

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-04.txt"
    let phrases = Text.lines text
    print $ part1 phrases
