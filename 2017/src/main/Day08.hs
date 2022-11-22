{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day08 where

import Common (readInputFileUtf8)
import Control.Applicative (some)
import Data.Foldable (asum)
import Data.Hashable (Hashable)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Data.Void (Void)
import Flow ((.>))
import System.Exit (die)
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as Parse
import Text.Megaparsec.Char qualified as Parse.Char
import Text.Megaparsec.Char.Lexer qualified as Parse.Lex


type Parser = Parsec Void Text

data Operator = Inc | Dec
    deriving (Bounded, Enum, Eq, Ord, Show)

data Condition l r
    = l :<:  r
    | l :<=: r
    | l :>:  r
    | l :>=: r
    | l :==: r
    | l :!=: r
    deriving (Eq, Ord, Show)

data Instr name val = MkInstr
    { register :: name
    , operator :: Operator
    , value :: val
    , condition :: Condition name val }
    deriving (Eq, Ord, Show)

parseInstrs :: Parser (Vector (Instr Text Int))
parseInstrs = Vec.fromList <$> Parse.sepEndBy parseInstr Parse.Char.eol

parseInstr :: Parser (Instr Text Int)
parseInstr = do
    register <- Text.pack <$> some Parse.Char.letterChar <* Parse.Char.hspace
    operator <- parseOperator <* Parse.Char.hspace
    value <- parseInteger <* Parse.Char.hspace
    Parse.Char.string "if" *> Parse.Char.hspace
    condition <- parseCondition <* Parse.Char.hspace
    pure $ MkInstr{..}

parseInteger :: Num a => Parser a
parseInteger = Parse.Lex.signed Parse.Char.hspace Parse.Lex.decimal

parseOperator :: Parser Operator
parseOperator = asum
    [ Dec <$ Parse.Char.string "dec"
    , Inc <$ Parse.Char.string "inc" ]

parseCondition :: Parser (Condition Text Int)
parseCondition = do
    left <- Text.pack <$> some Parse.Char.letterChar <* Parse.Char.hspace
    -- order matters here (make sure to try parsing @>=@ before @>@ and so on);
    -- also, avoid consuming input characters that may be needed for other
    -- alternatives
    cond <- asum
        [ (:<=:) <$ Parse.try (Parse.Char.string "<=")
        , (:<:)  <$ Parse.Char.string "<"
        , (:>=:) <$ Parse.try (Parse.Char.string ">=")
        , (:>:)  <$ Parse.Char.string ">"
        , (:==:) <$ Parse.Char.string "=="
        , (:!=:) <$ Parse.Char.string "!=" ]
    right <- Parse.Char.hspace *> parseInteger
    pure $ cond left right

evalOperator :: Num a => a -> Operator -> a -> a
evalOperator x = \case
    Inc -> (+ x)
    Dec -> subtract x

evalInstr
    :: (Eq k, Hashable k, Num v, Ord v)
    => Instr k v -> HashMap k v -> HashMap k v
evalInstr MkInstr{..} table = case condition of
    leftKey :<:  right -> eval leftKey (<)  right
    leftKey :<=: right -> eval leftKey (<=) right
    leftKey :>:  right -> eval leftKey (>)  right
    leftKey :>=: right -> eval leftKey (>=) right
    leftKey :==: right -> eval leftKey (==) right
    leftKey :!=: right -> eval leftKey (/=) right
  where
    alter = fromMaybe 0 .> evalOperator value operator .> Just
    eval leftKey op right =
        let left = HashMap.findWithDefault 0 leftKey table
        in  if left `op` right
                then HashMap.alter alter register table
                else table

evalInstrs
    :: (Eq k, Hashable k, Num v, Ord v)
    => Vector (Instr k v) -> HashMap k v -> HashMap k v
evalInstrs instrs table =
    -- note that it matters that we use a left fold here to evaluate the
    -- instructions from left-to-right - a right fold would do the opposite,
    -- evaluating right-to-left, and the results would generally differ as
    -- order matters here
    Vec.foldl' (flip evalInstr) table instrs

part1 :: Vector (Instr Text Int) -> Int
part1 = flip evalInstrs HashMap.empty .> HashMap.elems .> \case
    []     -> 0
    n : ns -> List.foldl' max n ns

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-08.txt"
    case Parse.runParser (parseInstrs <* Parse.eof) "day 08" text of
        Left err -> die $ Parse.errorBundlePretty err
        Right instrs -> do
            print $ part1 instrs
