module Asm
  ( Asm(..), runAsm, Temps(..),
  ) where

import Control.Monad (ap,liftM)
import Instruction (Code,Instruction)
import Semantics (ZeroPage,SemState,Flag)

instance Functor Asm where fmap = liftM
instance Applicative Asm where pure = Ret; (<*>) = ap
instance Monad Asm where (>>=) = Bind

data Asm a where
  Ret :: a -> Asm a
  Bind :: Asm a -> (a -> Asm b) -> Asm b
  Alt :: Asm a -> Asm a -> Asm a
  Nope :: Asm a
  FreshTemp :: Asm ZeroPage
  Emit :: Instruction -> Asm ()
  Query :: Asm SemState
  Update :: (SemState -> (a,SemState)) -> Asm a
  Branch :: Flag -> Asm a -> Asm a -> Asm a

runAsm :: Temps -> SemState -> Asm () -> [Code]
runAsm temps0 ss0 asm0 = [ code | (code,_s,()) <- run s0 asm0 ]
  where
    s0 = State { ss = ss0, temps = temps0 }

    run :: State -> Asm a -> [(Code,State,a)]
    run s@State{ss,temps} = \case
      Ret a -> [([],s,a)]

      Bind m f -> do
        [(c1++c2,s,b) | (c1,s,a) <- run s m
                      , (c2,s,b) <- run s (f a) ]

      Alt m1 m2 -> do
        run s m1 ++ run s m2

      Nope -> do
        []

      FreshTemp -> do
        case temps of
          Temps [] -> error "run out of temps"
          Temps (z:zs) -> do
            let s' = s { temps = Temps zs }
            [([],s',z)]

      Emit instruction -> do
        [ ([instruction], s, ()) ]

      Query -> do
        [ ([], s, ss) ]

      Update f -> do
        let (x,ss') = f ss
        let s' = s { ss = ss' }
        [ ([], s', x) ]

      Branch{} -> undefined


data State = State { ss :: SemState, temps :: Temps }

data Temps = Temps [ZeroPage]
