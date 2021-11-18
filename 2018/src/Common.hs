module Common where

import Data.Bifunctor (first)
import Data.ByteString qualified as Byt
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Enc
import Data.Void (Void)
import Flow ((.>))
import Text.Megaparsec qualified as Par


readFileUtf8 :: FilePath -> IO Text
readFileUtf8 path = Text.Enc.decodeUtf8 <$> Byt.readFile path

type Parser a = Par.Parsec Void Text a

runParser :: Parser a -> String -> Text -> Either String a
runParser parser desc = Par.parse parser desc .> first Par.errorBundlePretty
