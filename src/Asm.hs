module Asm
  ( AsmState(..), Asm(..), runAsm
  ) where

import Control.Monad (ap,liftM)
import Instruction (Code,Instruction)
import Semantics (ZeroPage,SemState,Flag)
import qualified Instruction as I (Instruction(Branch))

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

type QISF q a r = q -> ((r -> r) -> q -> a -> [r] -> [r]) -> [r] -> [r]

runAsm :: AsmState -> Asm () -> [Code]
runAsm q0 asm0 = run asm0 q0 (\i _q () f -> i [] : f) []
  where

    run :: Asm a -> QISF AsmState a Code
    run = \case
      Emit i -> \q s f -> s (i:) q () f
      Ret a -> \q s f -> s id q a f
      Bind m g -> \q s f -> run m q (\i1 q a f -> run (g a) q (\i2 q b f -> s (i1.i2) q b f) f) f
      Alt m1 m2 -> \q s f -> run m1 q s (run m2 q s f)
      Nope -> \_q _s f -> f

      Update m -> \q s f -> do
        let (x,q') = m q
        s id q' x f

      Branch _ m1 m2 -> \q s f -> do
        [ I.Branch c1 c2 | c1 <- run m1 q s [], c2 <- run m2 q s [] ] : f
