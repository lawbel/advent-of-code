{-# LANGUAGE FlexibleContexts #-}

module Common.State.Trans where

import Control.Monad.State (MonadState, get, put)
import Optics (Optic', Is, A_Setter)
import Optics.State.Operators ((%=))


infix 4 +=
(+=) :: (Is k A_Setter, MonadState s m, Num a) => Optic' k is s a -> a -> m ()
optic += x = optic %= (+ x)

infix 4 <>=
(<>=)
    :: (Is k A_Setter, MonadState s m, Semigroup a)
    => Optic' k is s a
    -> a
    -> m ()
optic <>= x = optic %= (<> x)

pop :: MonadState [a] m => m (Maybe a)
pop = get >>= \case
    []     -> pure Nothing
    x : xs -> Just x <$ put xs
