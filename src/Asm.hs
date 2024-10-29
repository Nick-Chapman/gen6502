module Asm
  ( AsmState(..), makeAsmState, updateSS, freshName
  , Asm(..), runAsm
  ) where

import Architecture (ZeroPage,Flag)
import Control.Monad (ap,liftM)
import Instruction (Code,Instruction)
import Semantics (Name(..),SemState)
import qualified Instruction as I (Code(..),Instruction(Branch))

----------------------------------------------------------------------
-- AsmState

data AsmState = AsmState
  { ss :: SemState
  , temps :: [ZeroPage]
  , u :: Int -- counter for fresh names
  }

makeAsmState :: SemState -> [ZeroPage] -> Int -> AsmState
makeAsmState ss temps u = AsmState { ss, temps, u  }

freshName :: Asm Name
freshName = Update f
  where f s@AsmState { u } = (NameU {unique = u}, s { u = u + 1 })

updateSS :: (SemState -> (a,SemState)) -> Asm a
updateSS m =
  Update (\s -> do
             let AsmState {ss} = s
             let (a,ss') = m ss
             let s' = s { ss = ss' }
             (a,s'))

----------------------------------------------------------------------
-- Asm

instance Functor Asm where fmap = liftM
instance Applicative Asm where pure = Ret; (<*>) = ap
instance Monad Asm where (>>=) = Bind

data Asm a where
  Io :: IO a -> Asm a
  Ret :: a -> Asm a
  Bind :: Asm a -> (a -> Asm b) -> Asm b
  Alt :: Asm a -> Asm a -> Asm a
  Nope :: Asm a
  Emit :: Instruction -> Asm ()
  Update :: (AsmState -> (a,AsmState)) -> Asm a
  Branch :: Flag -> Asm a -> Asm a -> Asm a

runAsm :: AsmState -> Asm () -> IO [Code]
runAsm q0 asm0 = do
  ns <- run asm0 q0 (\_q a -> pure [NDone a])
  pure [ untree t
       | n <- ns
       , t <- determinise n
       ]
  where
    run :: forall a r. Asm a -> AsmState -> (AsmState -> a -> IO [NCodeTree r]) -> IO [NCodeTree r]
    run = \case
      Io io -> \q k -> do a <- io; k q a
      Bind m g -> \q k -> run m q $ \q a -> run (g a) q k
      Ret a -> \q k -> k q a
      Emit i -> \q k -> do ns <- k q (); pure [NDo i ns]
      Branch flag m1 m2 -> \q k -> do ns1 <- run m1 q k; ns2 <- run m2 q k; pure [NBr flag ns1 ns2]
      Alt m1 m2 -> \q k -> do ns1 <- run m1 q k; ns2 <- run m2 q k; pure (ns1 ++ ns2)
      Nope -> \_q _k -> pure []
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
  Done () -> I.Done
  Do i t -> I.Do i (untree t)
  Br flag t1 t2 -> I.Do (I.Branch flag (untree t1) (untree t2)) I.Done
