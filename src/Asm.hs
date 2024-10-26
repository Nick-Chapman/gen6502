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

runAsm :: AsmState -> Asm () -> [Code]
runAsm q0 asm0 =
  [ untree t
  | n <- run asm0 q0 (\_q a -> [NDone a])
  , t <- determinise n
  ]
  where
    run :: forall a r. Asm a -> AsmState -> (AsmState -> a -> [NCodeTree r]) -> [NCodeTree r]
    run = \case
      Bind m g -> \q k -> run m q $ \q a -> run (g a) q k
      Ret a -> \q k -> k q a
      Emit i -> \q k -> [NDo i (k q ())]
      Branch flag m1 m2 -> \q k -> [NBr flag (run m1 q k) (run m2 q k)]
      Alt m1 m2 -> \q k -> run m1 q k ++ run m2 q k
      Nope -> \_q _k -> []
      Update m -> \q k -> let (x,q') = m q in k q' x


data NCodeTree a = NDone a | NDo Instruction [NCodeTree a] | NBr Flag [NCodeTree a] [NCodeTree a]

determinise :: NCodeTree a -> [CodeTree a]
determinise = \case
  NDone a -> [Done a]
  NDo i ns -> [ Do i c | n <- ns, c <- determinise n ]
  NBr flag ns1 ns2 -> [ Br flag c1 c2 | n1 <- ns1, c1 <- determinise n1, n2 <- ns2, c2 <- determinise n2 ]


data CodeTree a = Done a | Do Instruction (CodeTree a) | Br Flag (CodeTree a) (CodeTree a)

untree :: CodeTree () -> Code
untree = \case
  Done () -> []
  Do i t -> i : untree t
  Br flag t1 t2 -> [I.Branch flag (untree t1) (untree t2)]
