{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Day09 where

import Control.Monad.State (execState, State, MonadState, gets, evalState)
import Data.Bifunctor (first)
import Data.ByteString qualified as B
import Data.Char (intToDigit, digitToInt)
import Data.IntMap qualified as IM
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import Flow ((.>))
import Language.Haskell.Printf qualified as Printf
import Optics
    ((^.),  Optic, Is, A_Setter
    , noPrefixFieldLabels
    , makeFieldLabelsWith
    , view, at, (%)
    )
import Optics.State.Operators ((%=), (.=), (?=))


data ParamMode = Position | Immediate | Relative
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

data IntcodeState = MkIntcodeState
    { instrPtr :: Int
    , relBase :: Int
    , inputs :: [Int]
    , outputs :: [Int]
    , program :: IM.IntMap Int
    } deriving (Eq, Ord, Read, Show)

makeFieldLabelsWith noPrefixFieldLabels ''IntcodeState

main :: IO ()
main = do
    input <- parseInput <$> readFileUtf8 "day-09/input.txt"
    print $ part1 input

part1 :: IM.IntMap Int -> Int
part1 prog = case runIntcodeProg [1] prog of
    [x] -> x
    out -> error $ [Printf.s|part1: bad output: %?|] out

runIntcodeProg :: [Int] -> IM.IntMap Int -> [Int]
runIntcodeProg input prog =
    view #outputs $ execState interpretIntcodeProg $ MkIntcodeState
        { instrPtr = 0
        , relBase = 0
        , inputs = input
        , outputs = []
        , program = prog
        }

readPosAbs :: Int -> State IntcodeState Int
readPosAbs i = do
    prog <- gets (view #program)
    pure $ IM.findWithDefault 0 i prog

readPosRelToIPtr :: Int -> State IntcodeState Int
readPosRelToIPtr i = do
    iPtr <- gets (view #instrPtr)
    readPosAbs (iPtr + i)

writePos :: Int -> Int -> State IntcodeState ()
writePos i val = #program % at i ?= val

interpretIntcodeProg :: State IntcodeState ()
interpretIntcodeProg = do
    (opCode, paramModes) <- do
        iPtr <- gets (view #instrPtr)
        prog <- gets (view #program)
        pure $ getOpcodeAndParamModes (prog IM.! iPtr)

    let readParam i = case indexDefault Position paramModes (i - 1) of
            Immediate -> readPosRelToIPtr i
            Position -> readPosRelToIPtr i >>= readPosAbs
            Relative -> do
                rel <- gets (view #relBase)
                j <- readPosRelToIPtr i
                readPosAbs (j + rel)

    case opCode of
        n | n `elem` [1, 2] -> do
            let op = if n == 1 then (+) else (*)
            x <- op <$> readParam 1 <*> readParam 2
            dest <- readPosRelToIPtr 3
            writePos dest x
            #instrPtr += 4
            interpretIntcodeProg

        3 -> do
            ~(i : is) <- gets (view #inputs)
            dest <- readPosRelToIPtr 1
            writePos dest i
            #inputs .= is
            #instrPtr += 2
            interpretIntcodeProg

        4 -> do
            x <- readParam 1
            #outputs %= cons x
            #instrPtr += 2
            interpretIntcodeProg

        n | n `elem` [5, 6] -> do
            zero <- (== 0) <$> readParam 1
            if zero `xor` (n == 5)
                then #instrPtr += 3
                else do
                    ptr <- readParam 2
                    #instrPtr .= ptr
            interpretIntcodeProg

        n | n `elem` [7, 8] -> do
            let op = if n == 7 then (<) else (==)
                runOp x y = if x `op` y then 1 else 0
            b <- runOp <$> readParam 1 <*> readParam 2
            dest <- readPosRelToIPtr 3
            writePos dest b
            #instrPtr += 4
            interpretIntcodeProg

        9 -> do
            rel <- readParam 1
            #relBase += rel
            #instrPtr += 2
            interpretIntcodeProg

        99 -> #outputs %= reverse

        _ -> error $
            [Printf.s|interpretIntcodeProg: unexpected opcode %i|] opCode

getOpcodeAndParamModes :: Int -> (Int, [ParamMode])
getOpcodeAndParamModes instr =
    (parseDigits opCode, fmap mkParamMode paramModes)
  where
    (opCode, paramModes) =
        first reverse $ splitAt 2 $ reverse $ getDigits instr
    mkParamMode p
        | p == 0 = Position
        | p == 1 = Immediate
        | p == 2 = Relative
        | otherwise = error $
            [Printf.s|getOpcodeAndParamModes:
  while parsing instruction %i:
    encountered unexpected param mode %i|]
                instr p

getDigits :: Int -> [Int]
getDigits = show .> fmap digitToInt

parseDigits :: [Int] -> Int
parseDigits = fmap intToDigit .> read

indexDefault :: a -> [a] -> Int -> a
indexDefault x list n = case list of
    [] -> x
    y:ys -> if n == 0 then y else indexDefault x ys (n - 1)

cons :: a -> [a] -> [a]
cons = (:)

xor :: Bool -> Bool -> Bool
xor p q = (p || q) && not (p && q)

(+=)
    :: (Is k A_Setter, MonadState s m, Num a)
    => Optic k is s s a a
    -> a
    -> m ()
optic += x = optic %= (+ x)

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 path = TE.decodeUtf8 <$> B.readFile path

parseInput :: T.Text -> IM.IntMap Int
parseInput = T.splitOn "," .> fmap readInt .> zip [0..] .> IM.fromList
  where
    readInt :: T.Text -> Int
    readInt txt = case TR.signed TR.decimal txt of
        Left _ -> error "parseInput: input not an Int"
        Right (n, _) -> n
