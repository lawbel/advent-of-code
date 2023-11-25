module Day02 where

import Control.Applicative (some)
import Control.Monad (guard)
import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Flow ((<.))
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch

import Common


parseID :: Parser String
parseID = some Par.Ch.letterChar

parseIDs :: Parser [String]
parseIDs = parseID `Par.sepEndBy` Par.Ch.newline

getInput :: Text -> Either String [String]
getInput = runParser (parseIDs <* Par.eof) "day-02"

count :: Ord a => [a] -> Map a Int
count = foldr insert Map.empty
  where
    insert x = Map.insertWith (+) x 1

checksum :: Ord a => [[a]] -> Int
checksum input = numExactly 2 input * numExactly 3 input
  where
    numExactly n = length <. filter (exactly n)
    exactly n = elem n <. count

part1 :: [String] -> Int
part1 = checksum

numPairwiseNeq :: Eq a => [a] -> [a] -> Int
numPairwiseNeq xs ys = length $ filter neq $ zip xs ys
  where
    neq = uncurry (/=)

filterPairwiseEq :: Eq a => [a] -> [a] -> [a]
filterPairwiseEq xs ys = map fst $ filter eq $ zip xs ys
  where
    eq = uncurry (==)

part2 :: [String] -> Either String String
part2 inputs = maybeToEither "no matches" $ listToMaybe $ do
    input1 <- inputs
    input2 <- inputs
    guard $ numPairwiseNeq input1 input2 == 1
    pure $ filterPairwiseEq input1 input2

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-02.txt"
    case getInput text of
        Left err -> putStrLn err
        Right input -> do
            part1 input & print
            part2 input & either (mappend "error: ") id & putStrLn
