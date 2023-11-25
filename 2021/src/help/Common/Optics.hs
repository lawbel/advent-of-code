{-# LANGUAGE FlexibleContexts #-}

module Common.Optics where

import Control.Monad.Reader (MonadReader, asks)
import Flow ((<.))
import Linear (R1, R2, R3)
import Linear qualified
import Optics
    ( Is, A_Setter, Optic, Lens', A_Getter, Optic', An_AffineFold, (%~) )
import Optics qualified


infixr 4 +~
(+~) :: (Is k A_Setter, Num a) => Optic k is s t a a -> a -> s -> t
optic +~ x = optic %~ (+ x)

infixr 4 -~
(-~) :: (Is k A_Setter, Num a) => Optic k is s t a a -> a -> s -> t
optic -~ x = optic %~ subtract x

infixr 4 <>~
(<>~) :: (Is k A_Setter, Semigroup a) => Optic k is s t a a -> a -> s -> t
optic <>~ x = optic %~ (<> x)

_x :: R1 f => Lens' (f a) a
_x = Optics.lensVL Linear._x

_y :: R2 f => Lens' (f a) a
_y = Optics.lensVL Linear._y

_z :: R3 f => Lens' (f a) a
_z = Optics.lensVL Linear._z

seek :: (Is k A_Getter, MonadReader s m) => Optic' k is s a -> m a
seek optic = asks (Optics.view optic)

is :: Is k An_AffineFold => Optic' k is s a -> s -> Bool
is optic = not <. Optics.isn't optic
