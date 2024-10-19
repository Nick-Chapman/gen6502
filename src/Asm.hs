module Asm
  ( Asm(..), runAsm, Temps(..), Cost,
  ) where

import Control.Monad (ap,liftM)
import Cost (Cost,cost)
import Data.List (sortBy)
import Instruction (Code,Instruction)
import Semantics (ZeroPage,SemState,Flag)

import qualified Cost

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

type CostOrdering = Cost -> Cost -> Ordering

runAsm :: CostOrdering -> Temps -> SemState -> Asm a -> [(Code,Cost,a)]
runAsm costOrdering temps0 ss0 asm0 = do

  sortByCost [ (code,q,a) | (code,q,_s,a) <- run s0 asm0 ]
  where
    doSort = True
    -- TODO: produce results in cost order, rather than post-sorting
    sortByCost =
      if doSort
      then sortBy (\(code1,c1,_) (code2,c2,_) ->
                     case costOrdering c1 c2 of
                       EQ -> compare code1 code2 -- order determinism of tests
                       x -> x
                  )
      else id

    s0 = State { ss = ss0, temps = temps0 }

    (+) = Cost.add
    zero = Cost.zero

    run :: State -> Asm a -> [(Code,Cost,State,a)]
    run s@State{ss,temps} = \case
      Ret a -> [([],zero,s,a)]

      Bind m f -> do
        [(c1++c2,q1+q2,s,b) | (c1,q1,s,a) <- run s m
                            , (c2,q2,s,b) <- run s (f a) ]

      Alt m1 m2 -> do
        run s m1 ++ run s m2

      Nope -> do
        []

      FreshTemp -> do
        case temps of
          Temps [] -> error "run out of temps"
          Temps (z:zs) -> do
            let s' = s { temps = Temps zs }
            [([],zero,s',z)]

      Emit instruction -> do
        [ ([instruction], cost instruction, s, ()) ]

      Query -> do
        [ ([], zero, s, ss) ]

      Update f -> do
        let (x,ss') = f ss
        let s' = s { ss = ss' }
        [ ([], zero, s', x) ]

      Branch{} -> undefined


data State = State { ss :: SemState, temps :: Temps }

data Temps = Temps [ZeroPage]
