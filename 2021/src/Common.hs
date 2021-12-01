module Common where

import Control.Monad ((>=>))
import Data.Bifunctor (first)
import Data.ByteString qualified as Byt
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Enc
import Data.Void (Void)
import Flow ((.>))
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as Par

import Paths_adventofcode2021 (getDataFileName)


type Parser a = Parsec Void Text a

runParser :: Parser a -> String -> Text -> Either String a
runParser parser desc = Par.parse parser desc .> first Par.errorBundlePretty

readFileUtf8 :: FilePath -> IO Text
readFileUtf8 path = Text.Enc.decodeUtf8 <$> Byt.readFile path

readInputFileUtf8 :: FilePath -> IO Text
readInputFileUtf8 = getDataFileName >=> readFileUtf8
