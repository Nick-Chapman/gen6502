module Asm
  ( Asm(..), runAsm, Temps(..), Cost
  ) where

import Control.Monad (ap,liftM)
import Instruction (Code,Instruction,ZeroPage,Semantics,SemState)

instance Functor Asm where fmap = liftM
instance Applicative Asm where pure = Ret; (<*>) = ap
instance Monad Asm where (>>=) = Bind

data Asm a where
  Ret :: a -> Asm a
  Bind :: Asm a -> (a -> Asm b) -> Asm b
  Alt :: Asm a -> Asm a -> Asm a
  Nope :: Asm a
  GetSemState :: Asm SemState
  SetSemState :: SemState -> Asm ()
  Fresh :: Asm ZeroPage
  Emit :: Instruction -> Semantics -> Asm ()

runAsm :: Temps -> SemState -> Asm a -> [(Code,Cost,a)]
runAsm temps0 ss0 asm0 = [ (code,q,a) | (code,q,_s,a) <- loop s0 asm0 ]
  where
    s0 = State { ss = ss0, temps = temps0 }

    loop :: State -> Asm a -> [(Code,Cost,State,a)]
    loop s@State{ss,temps} = \case
      Ret a -> [([],0,s,a)]
      Bind m f ->
        [(c1++c2,q1+q2,s,b) | (c1,q1,s,a) <- loop s m
                            , (c2,q2,s,b) <- loop s (f a) ]

      Alt m1 m2 -> loop s m1 ++ loop s m2
      Nope -> []

      GetSemState -> do
        [([],0,s,ss)]

      SetSemState ss -> do
        let s' = s { ss }
        [([],0,s',())]

      Fresh -> do
        case temps of
          Temps [] -> error "run out of temps"
          Temps (z:zs) -> do
            let s' = s { temps = Temps zs }
            [([],0,s',z)]

      Emit instruction semantics -> do
        let s' = s { ss = semantics ss }
        [ ([instruction], cost instruction, s', ()) ]


data State = State { ss :: SemState, temps :: Temps }

data Temps = Temps [ZeroPage]

----------------------------------------------------------------------
-- cost

newtype Cost = Cost Int deriving (Num)

cost :: Instruction -> Cost
cost _ = 1 -- TEMP -- better cycles or space

instance Show Cost where
  show (Cost n) = show n
