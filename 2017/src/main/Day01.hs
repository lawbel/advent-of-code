{-# LANGUAGE QuasiQuotes #-}

module Day01 where

import Common
import Data.Char qualified as Char
import Data.Text qualified as Text
import System.OsPath qualified as Sys


shiftL :: Int -> [a] -> [a]
shiftL n
    | n > 0 = \case
        []     -> []
        x : xs -> shiftL (n - 1) (xs <> [x])
    | otherwise = id

captcha :: (Num a, Eq a) => [a] -> a
captcha xs = sum $ zipWith (\l r -> if l == r then l else 0) xs (shiftL 1 xs)

captcha2 :: (Num a, Eq a) => [a] -> a
captcha2 xs =
    let n = length xs `div` 2
    in  sum $ zipWith (\l r -> if l == r then l else 0) xs (shiftL n xs)

main :: IO ()
main = do
    text <- readInputFileUtf8 [Sys.osp|input/day-01.txt|]
    let chars = Text.unpack $ Text.strip text
    let digits = Char.digitToInt <$> chars
    print $ captcha digits
    print $ captcha2 digits
