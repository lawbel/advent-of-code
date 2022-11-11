{-# LANGUAGE QuasiQuotes #-}

module Day01 where

import Common
import Data.Char qualified as Char
import Data.Text qualified as Text
import System.OsPath qualified as Sys


captcha :: (Num a, Eq a) => [a] -> a
captcha = \case
    [] -> 0
    list@(x : xs) ->
        let shifted = xs <> [x]
        in  sum $ zipWith (\l r -> if l == r then l else 0) list shifted

main :: IO ()
main = do
    text <- readInputFileUtf8 [Sys.osp|input/day-01.txt|]
    let chars = Text.unpack $ Text.strip text
    let digits = Char.digitToInt <$> chars
    print $ captcha digits
