module Asm
  ( Asm(..), runAsm, Temps(..),
  ) where

import Control.Monad (ap,liftM)
import Instruction (Code,Instruction)
import Semantics (ZeroPage,SemState,Flag)

----------------------------------------------------------------------
-- State

data State = State { ss :: SemState, temps :: Temps }
data Temps = Temps [ZeroPage]

runAsm :: Temps -> SemState -> Asm () -> [Code] -- TODO: unfold this wrapper into calling code
runAsm temps ss asm = runAsm' (makeState temps ss) asm

makeState :: Temps -> SemState -> State
makeState temps ss = State { ss, temps }

----------------------------------------------------------------------
-- Asm

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

type EQSF i q a r = [i] -> q -> ([i] -> q -> a -> r -> r) -> r -> r

runAsm' :: State -> Asm () -> [Code]
runAsm' q0 asm0 = run asm0 [] q0 (\is _q () f -> reverse is : f) []
  where

    run :: Asm a -> EQSF Instruction State a [Code]
    run = \case
      Ret a -> \is q s f -> s is q a f
      Bind m g -> \is q s f -> run m is q (\is q a f -> run (g a) is q s f) f
      Alt m1 m2 -> \is q s f -> run m1 is q s (run m2 is q s f)
      Nope -> \_is _q _s f -> f
      Emit i -> \is q s f -> s (i:is) q () f

      Query -> \is q s f -> do
        let State{ss} = q
        s is q ss f

      FreshTemp -> \is q s f -> do
        let State{temps} = q
        case temps of
          Temps [] -> error "run out of temps"
          Temps (firstTemp:rest) -> do
            let q' = q { temps = Temps rest }
            s is q' firstTemp f

      Update m -> \is q s f -> do
        let State{ss} = q
        let (x,ss') = m ss
        let q' = q { ss = ss' }
        s is q' x f

      Branch{} -> undefined
