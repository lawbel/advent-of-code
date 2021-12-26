{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Common.State.Poly where

import Optics (A_Setter, A_Getter, Is, Optic', Optic, (.~), (%~), view)
import Polysemy (Member, Sem)
import Polysemy.State (State, modify, gets)


use :: (Is k A_Getter, Member (State s) r) => Optic' k is s a -> Sem r a
use optic = gets (view optic)

infix 4 .=
(.=)
    :: (Is k A_Setter, Member (State s) r)
    => Optic k is s s a b -> b -> Sem r ()
optic .= x = modify (optic .~ x)

infix 4 +=
(+=)
    :: (Is k A_Setter, Member (State s) r, Num a)
    => Optic' k is s a -> a -> Sem r ()
optic += x = optic %= (+ x)

infix 4 *=
(*=)
    :: (Is k A_Setter, Member (State s) r, Num a)
    => Optic' k is s a -> a -> Sem r ()
optic *= x = optic %= (* x)

infix 4 %=
(%=)
    :: (Is k A_Setter, Member (State s) r)
    => Optic k is s s a b -> (a -> b) -> Sem r ()
optic %= f = modify (optic %~ f)
