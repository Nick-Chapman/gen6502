module Asm
  ( AsmState(..), Asm(..), runAsm
  ) where

import Control.Monad (ap,liftM)
import Instruction (Code,Instruction)
import Semantics (ZeroPage,SemState,Flag)

----------------------------------------------------------------------
-- AsmState

data AsmState = AsmState { ss :: SemState, temps :: [ZeroPage] }

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
  Emit :: Instruction -> Asm ()
  Update :: (AsmState -> (a,AsmState)) -> Asm a
  Branch :: Flag -> Asm a -> Asm a -> Asm a

type EQSF i q a r = [i] -> q -> ([i] -> q -> a -> r -> r) -> r -> r

runAsm :: AsmState -> Asm () -> [Code]
runAsm q0 asm0 = run asm0 [] q0 (\is _q () f -> reverse is : f) []
  where

    run :: Asm a -> EQSF Instruction AsmState a [Code]
    run = \case
      Ret a -> \is q s f -> s is q a f
      Bind m g -> \is q s f -> run m is q (\is q a f -> run (g a) is q s f) f
      Alt m1 m2 -> \is q s f -> run m1 is q s (run m2 is q s f)
      Nope -> \_is _q _s f -> f
      Emit i -> \is q s f -> s (i:is) q () f

      Update m -> \is q s f -> do
        let (x,q') = m q
        s is q' x f

      Branch{} -> undefined
