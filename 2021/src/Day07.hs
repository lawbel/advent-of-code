module Day07 where

import Control.Applicative.Combinators.NonEmpty qualified as Par.NE
import Data.Foldable (minimumBy)
import Data.List.NonEmpty (NonEmpty)
import Data.Ord (comparing)
import Data.Text (Text)
import Flow ((.>))
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch
import Text.Megaparsec.Char.Lexer qualified as Par.Ch.Lex

import Common


parseCrabs :: Num a => Parser (NonEmpty a)
parseCrabs = Par.Ch.Lex.decimal `Par.NE.sepBy1` Par.Ch.char ','

getCrabs :: Num a => Text -> Either String (NonEmpty a)
getCrabs = runParser "day-07" $ parseCrabs <* Par.Ch.space <* Par.eof

centre :: (Enum a, Ord a, Num a) => NonEmpty a -> (a, a)
centre xs = minimumBy (comparing snd) $ do
    i <- [minimum xs .. maximum xs]
    let diff x = abs $ x - i
    pure (i, sum $ fmap diff xs)

part1 :: (Enum a, Ord a, Num a) => NonEmpty a -> a
part1 = centre .> snd

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-07.txt"
    case getCrabs @Int text of
        Left err -> die err
        Right crabs -> do
            print $ part1 crabs
