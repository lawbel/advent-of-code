{-# LANGUAGE LambdaCase #-}

module Common where

import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.ByteString qualified as Byt
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Enc
import Data.Void (Void)
import Flow ((.>))
import Text.Megaparsec qualified as Par

import Paths_adventofcode2018 (getDataFileName)


readFileUtf8 :: FilePath -> IO Text
readFileUtf8 path = Text.Enc.decodeUtf8 <$> Byt.readFile path

readInputFileUtf8 :: FilePath -> IO Text
readInputFileUtf8 = getDataFileName >=> readFileUtf8

type Parser a = Par.Parsec Void Text a

runParser :: Parser a -> String -> Text -> Either String a
runParser parser desc = Par.parse parser desc .> first Par.errorBundlePretty

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither def = \case
    Nothing -> Left def
    Just x  -> Right x
