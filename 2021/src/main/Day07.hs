module Day07 where

import Control.Applicative.Combinators.NonEmpty qualified as App.NE
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
parseCrabs = Par.Ch.Lex.decimal `App.NE.sepBy1` Par.Ch.char ','

getCrabs :: Num a => Text -> Either String (NonEmpty a)
getCrabs = runParser "day-07" $ parseCrabs <* Par.Ch.space <* Par.eof

triangle :: Integral a => a -> a
triangle n = (n * (n + 1)) `div` 2

centerOn :: Integral a => (a -> a -> a) -> NonEmpty a -> (a, a)
centerOn f xs = minimumBy (comparing snd) $ do
    i <- [minimum xs .. maximum xs]
    pure (i, sum $ fmap (f i) xs)

center1 :: Integral a => NonEmpty a -> (a, a)
center1 = centerOn $ \x y -> abs $ x - y

part1 :: Integral a => NonEmpty a -> a
part1 = center1 .> snd

center2 :: Integral a => NonEmpty a -> (a, a)
center2 = centerOn $ \x y -> triangle $ abs $ x - y

part2 :: Integral a => NonEmpty a -> a
part2 = center2 .> snd

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-07.txt"
    case getCrabs @Int text of
        Left err -> die err
        Right crabs -> do
            print $ part1 crabs
            print $ part2 crabs
