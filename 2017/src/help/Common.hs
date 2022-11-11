module Common where

import Control.Monad ((>=>))
import Data.ByteString qualified as Byte
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Enc
import Flow ((.>))
import System.OsPath (OsPath)
import System.OsPath qualified as Sys

import Paths_adventofcode2017 (getDataFileName)


the :: a -> a
the = id

readFileUtf8 :: OsPath -> IO Text
readFileUtf8 = Sys.decodeUtf >=> Byte.readFile >=> pure . Text.Enc.decodeUtf8

readInputFileUtf8 :: OsPath -> IO Text
readInputFileUtf8 =
    Sys.decodeUtf >=> getDataFileName >=> Sys.encodeUtf >=> readFileUtf8

tShow :: Show a => a -> Text
tShow = show .> Text.pack
