module Common
    ( readFileUtf8
    , readInputFileUtf8

    , textShow
    , textRead

    , the
    , (<<$>>)
    ) where

import Control.Monad ((>=>))
import Data.ByteString qualified as Byte
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Enc
import Flow ((.>))
import Paths_adventofcode2017 (getDataFileName)
import System.OsPath (OsPath)
import System.OsPath qualified as Sys


-- | Can be a nice alternative to type annotations like @(5 :: Int)@ - instead
-- of that, with @-XTypeApplications@ turned on, one can write @the \@Int 5@.
the :: a -> a
the = id

-- | Same as '<$>' but works inside two levels of functors.
(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <<$>> xs = fmap f <$> xs

readFileUtf8 :: OsPath -> IO Text
readFileUtf8 = Sys.decodeUtf >=> Byte.readFile >=> pure . Text.Enc.decodeUtf8

readInputFileUtf8 :: OsPath -> IO Text
readInputFileUtf8 =
    Sys.decodeUtf >=> getDataFileName >=> Sys.encodeUtf >=> readFileUtf8

textShow :: Show a => a -> Text
textShow = show .> Text.pack

textRead :: Read a => Text -> a
textRead = Text.unpack .> read
