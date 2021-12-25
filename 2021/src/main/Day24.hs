{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Day24 where

import Control.Applicative (empty)
import Control.Monad (replicateM)
import Data.Foldable (asum)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Flow ((.>))
import Optics (Lens')
import Optics.TH (noPrefixFieldLabels, makeFieldLabelsWith)
import Polysemy (Sem, Members, Member, run)
import Polysemy.Error (Error, throw, runError)
import Polysemy.Input (Input, input, runInputList)
import Polysemy.State (State, execState)
import System.Exit (die)
import Text.Megaparsec qualified as Par
import Text.Megaparsec.Char qualified as Par.Ch
import Text.Megaparsec.Char.Lexer qualified as Par.Ch.Lex

import Common
import Common.State.Poly


-- types

data Register = W | X | Y | Z
    deriving (Eq, Ord, Read, Show)

data Registers a = MkRegisters
    { w :: a
    , x :: a
    , y :: a
    , z :: a }
    deriving (Eq, Ord, Read, Show)

makeFieldLabelsWith noPrefixFieldLabels ''Registers

data Instr a b
    = Inp a
    | Add a (Either a b)
    | Mul a (Either a b)
    | Div a (Either a b)
    | Mod a (Either a b)
    | Eql a (Either a b)
    deriving (Eq, Ord, Read, Show)

data Crash
    = EndOfInput
    | NegativeValue
    | ModIsZero
    | ModIsNegative
    deriving (Eq, Ord, Read, Show)

-- parsing

parseRegister :: Parser Register
parseRegister = asum
    [ W <$ Par.Ch.char' 'w'
    , X <$ Par.Ch.char' 'x'
    , Y <$ Par.Ch.char' 'y'
    , Z <$ Par.Ch.char' 'z' ]

parseValue :: Num a => Parser a
parseValue = Par.Ch.Lex.signed Par.Ch.hspace Par.Ch.Lex.decimal

parseRegisterOrValue :: Num a => Parser (Either Register a)
parseRegisterOrValue = asum
    [ Left <$> parseRegister
    , Right <$> parseValue ]

parseInstrInp :: Parser (Instr Register a)
parseInstrInp = do
    Par.Ch.string' "inp" *> Par.Ch.hspace
    val <- parseRegister <* Par.Ch.hspace
    pure $ Inp val

parseInstrBinary
    :: Num a
    => Text
    -> (Register -> Either Register a -> Instr Register a)
    -> Parser (Instr Register a)
parseInstrBinary text make = do
    Par.Ch.string' text *> Par.Ch.hspace
    val1 <- parseRegister <* Par.Ch.hspace
    val2 <- parseRegisterOrValue <* Par.Ch.hspace
    pure $ make val1 val2

parseInstr :: Num a => Parser (Instr Register a)
parseInstr = asum
    [ parseInstrInp
    , parseInstrBinary "add" Add
    , parseInstrBinary "mul" Mul
    , parseInstrBinary "div" Div
    , parseInstrBinary "mod" Mod
    , parseInstrBinary "eql" Eql ]

parseInstrs :: Num a => Parser [Instr Register a]
parseInstrs = parseInstr `Par.sepEndBy` Par.Ch.space

getInstrs :: Num a => Text -> Either String [Instr Register a]
getInstrs = runParser "day-24" $ parseInstrs <* Par.eof

-- core logic

register :: Register -> Lens' (Registers a) a
register = \case
    W -> #w
    X -> #x
    Y -> #y
    Z -> #z

resolve :: Member (State (Registers a)) r => Either Register a -> Sem r a
resolve = \case
    Left reg  -> use $ register reg
    Right val -> pure val

instr
    :: Members
        [ Error Crash
        , Input (Maybe a)
        , State (Registers a) ]
        r
    => Integral a
    => Instr Register a
    -> Sem r ()
instr = \case
    Inp a -> input >>= \case
        Nothing  -> throw EndOfInput
        Just val -> register a .= val
    Add a b -> do
        val <- resolve b
        register a += val
    Mul a b -> do
        val <- resolve b
        register a *= val
    Div a b -> do
        val <- resolve b
        if val == 0
            then throw ModIsZero
            else register a %= (`quot` val)
    Mod a b -> do
        cur <- use $ register a
        val <- resolve b
        if  | cur < 0   -> throw NegativeValue
            | val < 0   -> throw ModIsNegative
            | val == 0  -> throw ModIsZero
            | otherwise -> register a .= (cur `rem` val)
    Eql a b -> do
        cur <- use $ register a
        val <- resolve b
        register a .= (if cur == val then 1 else 0)

instrs
    :: Members
        [ Error Crash
        , Input (Maybe a)
        , State (Registers a) ]
        r
    => Integral a
    => [Instr Register a]
    -> Sem r ()
instrs = \case
    []     -> pure ()
    i : is -> instr i *> instrs is

exec
    :: r ~
        [ Input (Maybe a)
        , State (Registers b)
        , Error e ]
    => [a]
    -> Registers b
    -> Sem r ()
    -> Either e (Registers b)
exec as regs = runInputList as .> execState regs .> runError .> run

compute
    :: Integral a => [a] -> [Instr Register a] -> Either Crash (Registers a)
compute vals = instrs .> exec vals (MkRegisters 0 0 0 0)

-- parts & main hook

part1 :: Integral a => [Instr Register a] -> Maybe [a]
part1 is = listToMaybe $ do
    vals <- replicateM 14 [9, 8 .. 1]
    case compute vals is of
        Left _     -> empty
        Right regs -> case z regs of
            0 -> pure vals
            _ -> empty

main :: IO ()
main = do
    text <- readInputFileUtf8 "input/day-24.txt"
    case getInstrs @Int text of
        Left err -> die err
        Right is -> do
            print $ part1 is
