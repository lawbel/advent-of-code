module Day09 where

import Common (readInputFileUtf8)
import Control.Applicative (many, (<|>))
import Control.Lens (preview, _Right)
import Data.Foldable (asum)
import Data.Functor (void)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Data.Void (Void)
import System.Exit (die)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.Char


type Parser = Parsec Void Text

newtype Group = MkGroup (Vector (Either Garbage Group))
    deriving (Eq, Ord, Show)

newtype Garbage = MkGarbage Text
    deriving (Eq, Ord, Show)

parseGroup :: Parser Group
parseGroup = do
    void $ Parse.single '{'
    inner <- parseInner `Parse.sepBy` Parse.single ','
    void $ Parse.single '}'
    pure $ MkGroup $ Vec.fromList inner
  where
    parseInner = fmap Left parseGarbage <|> fmap Right parseGroup

parseGarbage :: Parser Garbage
parseGarbage = do
    void $ Parse.single '<'
    garbage <- many $ asum
        [ Text.singleton <$> Parse.noneOf ['!', '>']
        , Text.pack <$> do
            a <- Parse.single '!'
            b <- Parse.anySingle
            pure [a, b] ]
    void $ Parse.single '>'
    pure $ MkGarbage $ mconcat garbage

scoreGroups :: Group -> Int
scoreGroups = go 1
  where
    go :: Int -> Group -> Int
    go level (MkGroup inner) =
        let groups = Vec.mapMaybe (preview _Right) inner
        in  level + Vec.sum (Vec.map (go (level + 1)) groups)

part1 :: Group -> Int
part1 = scoreGroups

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-09.txt"
    case Parse.runParser parse "day 09" text of
        Left err -> die $ Parse.errorBundlePretty err
        Right group -> do
            print $ part1 group
  where
    parse = parseGroup <* Parse.Char.space <* Parse.eof
