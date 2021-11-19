module Day02 where

import Control.Applicative (some)
import Data.Map (Map)
import Data.Map qualified as Map
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

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-02.txt"
    case getInput text of
        Left err -> putStrLn err
        Right input -> do
            print $ part1 input
