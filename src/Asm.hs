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
runAsm temps0 ss0 asm0 = [ code | (code,_s,()) <- run asm0 q0 ]
  where
    q0 = State { ss = ss0, temps = temps0 }

    run :: Asm a -> State -> [(Code,State,a)]
    run = \case
      Ret a -> \q -> [([],q,a)]

      Bind m g -> \q -> do
        [(c1++c2,q,b) | (c1,q,a) <- run m q
                      , (c2,q,b) <- run (g a) q ]

      Alt m1 m2 -> \q -> do
        run m1 q ++ run m2 q

      Nope -> \_q -> do
        []

      FreshTemp -> \q -> do
        let State{temps} = q
        case temps of
          Temps [] -> error "run out of temps"
          Temps (z:zs) -> do
            let q' = q { temps = Temps zs }
            [([],q',z)]

      Emit instruction -> \q -> do
        [ ([instruction], q, ()) ]

      Query -> \q -> do
        let State{ss} = q
        [ ([], q, ss) ]

      Update f -> \q -> do
        let State{ss} = q
        let (x,ss') = f ss
        let q' = q { ss = ss' }
        [ ([], q', x) ]

      Branch{} -> undefined


data State = State { ss :: SemState, temps :: Temps }

data Temps = Temps [ZeroPage]
