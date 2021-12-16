{-# LANGUAGE FlexibleContexts #-}

module Common where

import Control.Monad ((>=>))
import Control.Monad.Reader (MonadReader, asks)
import Control.Monad.State (MonadState, get, put)
import Data.Bifunctor (first)
import Data.ByteString qualified as Byt
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Semigroup.Foldable (Foldable1)
import Data.Semigroup.Foldable qualified as Fold1
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Enc
import Data.Void (Void)
import Flow ((.>), (<.))
import Linear (R1, R2)
import Linear qualified
import Optics
    ( A_Setter, A_Getter, An_AffineFold, Is, Optic, Optic', Lens', (%~) )
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

textShow :: Show a => a -> Text
textShow = show .> Text.pack

cons :: a -> [a] -> [a]
cons x xs = x : xs

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither l = \case
    Nothing -> Left l
    Just r  -> Right r


(+~) :: (Is k A_Setter, Num a) => Optic k is s t a a -> a -> s -> t
optic +~ x = optic %~ (+ x)

(+=) :: (Is k A_Setter, MonadState s m, Num a) => Optic' k is s a -> a -> m ()
optic += x = optic %= (+ x)

(-~) :: (Is k A_Setter, Num a) => Optic k is s t a a -> a -> s -> t
optic -~ x = optic %~ subtract x

(<>~) :: (Is k A_Setter, Semigroup a) => Optic k is s t a a -> a -> s -> t
optic <>~ x = optic %~ (<> x)

(<>=)
    :: (Is k A_Setter, MonadState s m, Semigroup a)
    => Optic' k is s a
    -> a
    -> m ()
optic <>= x = optic %= (<> x)

_x :: R1 f => Lens' (f a) a
_x = Optics.lensVL Linear._x

_y :: R2 f => Lens' (f a) a
_y = Optics.lensVL Linear._y

seek :: (Is k A_Getter, MonadReader s m) => Optic' k is s a -> m a
seek optic = asks (Optics.view optic)

is :: Is k An_AffineFold => Optic' k is s a -> s -> Bool
is optic = not <. Optics.isn't optic


ffmap :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
ffmap = fmap .> fmap

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = ffmap

pop :: MonadState [a] m => m (Maybe a)
pop = get >>= \case
    []     -> pure Nothing
    x : xs -> Just x <$ put xs

compose :: Int -> (a -> a) -> a -> a
compose n f
    | n <= 0    = id
    | otherwise = f .> compose (n - 1) f


supremum1 :: Foldable1 f => Ord a => f a -> a
supremum1 = Fold1.toNonEmpty .> \(x :| xs) -> supremum x xs

supremum :: (Foldable f, Ord a) => a -> f a -> a
supremum = supremumBy compare

supremumBy1 :: Foldable1 f => (a -> a -> Ordering) -> f a -> a
supremumBy1 cmp = Fold1.toNonEmpty .> \(x :| xs) -> supremumBy cmp x xs

supremumBy :: Foldable f => (a -> a -> Ordering) -> a -> f a -> a
supremumBy cmp = foldl $
    \x y -> case x `cmp` y of
        GT -> x
        _  -> y

infimum1 :: (Foldable1 f, Ord a) => f a -> a
infimum1 = Fold1.toNonEmpty .> \(x :| xs) -> infimum x xs

infimum :: (Foldable f, Ord a) => a -> f a -> a
infimum = infimumBy compare

infimumBy1 :: Foldable1 f => (a -> a -> Ordering) -> f a -> a
infimumBy1 cmp = Fold1.toNonEmpty .> \(x :| xs) -> infimumBy cmp x xs

infimumBy :: Foldable f => (a -> a -> Ordering) -> a -> f a -> a
infimumBy cmp = foldl $
    \x y -> case x `cmp` y of
        LT -> x
        _  -> y
