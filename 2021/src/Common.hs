{-# LANGUAGE FlexibleContexts #-}

module Common where

import Control.Monad ((>=>))
import Control.Monad.State (MonadState, get, put)
import Data.Bifunctor (first)
import Data.ByteString qualified as Byt
import Data.Text (Text)
import Data.Text.Encoding qualified as Text.Enc
import Data.Void (Void)
import Flow ((.>))
import Linear (R1, R2)
import Linear qualified
import Optics (A_Setter, Is, Optic, Optic', Lens', (%~))
import Optics qualified
import Optics.State.Operators ((%=))
import Text.Megaparsec (Parsec)
import Text.Megaparsec qualified as Par

import Paths_adventofcode2021 (getDataFileName)


type Parser a = Parsec Void Text a

runParser :: String -> Parser a -> Text -> Either String a
runParser desc parser = Par.parse parser desc .> first Par.errorBundlePretty


readFileUtf8 :: FilePath -> IO Text
readFileUtf8 path = Text.Enc.decodeUtf8 <$> Byt.readFile path

readInputFileUtf8 :: FilePath -> IO Text
readInputFileUtf8 = getDataFileName >=> readFileUtf8


(+~) :: (Is k A_Setter, Num a) => Optic k is s t a a -> a -> s -> t
optic +~ x = optic %~ (+ x)

(-~) :: (Is k A_Setter, Num a) => Optic k is s t a a -> a -> s -> t
optic -~ x = optic %~ subtract x

(+=) :: (Is k A_Setter, MonadState s m, Num a) => Optic' k is s a -> a -> m ()
optic += x = optic %= (+ x)

_x :: R1 f => Lens' (f a) a
_x = Optics.lensVL Linear._x

_y :: R2 f => Lens' (f a) a
_y = Optics.lensVL Linear._y


ffmap :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
ffmap = fmap .> fmap

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = ffmap

fffmap
    :: (Functor f, Functor g, Functor h)
    => (a -> b)
    -> f (g (h a))
    -> f (g (h b))
fffmap = fmap .> fmap .> fmap

(<<<$>>>)
    :: (Functor f, Functor g, Functor h)
    => (a -> b)
    -> f (g (h a))
    -> f (g (h b))
(<<<$>>>) = fffmap


pop :: MonadState [a] m => m (Maybe a)
pop = get >>= \case
    []     -> pure Nothing
    x : xs -> Just x <$ put xs
