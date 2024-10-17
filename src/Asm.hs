module Asm
  ( Asm(..), runAsm, Temps(..), Cost
  ) where

import Control.Monad (ap,liftM)
import Cost (Cost,cost)
import Data.List (sortBy)
import Instruction (Code,Instruction)
import Semantics (ZeroPage,Semantics,SemState,Reg,findSemState,findSemOper,Name,getFreshName,Oper)

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
  FreshName :: Asm Name
  Emit :: Instruction -> Semantics -> Asm ()
  FindOper :: Oper -> Asm (Maybe Name)
  FindName :: Name -> Asm [Reg]
  Print :: String -> Asm ()

type CostOrdering = Cost -> Cost -> Ordering

runAsm :: CostOrdering -> Temps -> SemState -> Asm a -> IO [(Code,Cost,a)]
runAsm costOrdering temps0 ss0 asm0 = do

  xs <- loop s0 asm0
  pure (sortByCost [ (code,q,a) | (code,q,_s,a) <- xs ])
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

    loop :: State -> Asm a -> IO [(Code,Cost,State,a)]
    loop s@State{ss,temps} = \case
      Ret a -> pure [([],zero,s,a)]

      Bind m f -> do
        xs1 <- loop s m
--        [(c1++c2,q1+q2,s,b) | (c1,q1,s,a) <- loop s m
--                            , (c2,q2,s,b) <- loop s (f a) ]

        xss <- sequence [ do
              xs2 <- loop s (f a)
              pure [ (c1++c2,q1+q2,s,b) | (c2,q2,s,b) <- xs2 ]
          | (c1,q1,s,a) <- xs1
          ]
        pure (concat xss)

      Alt m1 m2 -> do
        rs1 <- loop s m1
        rs2 <- loop s m2
        pure (rs1 ++ rs2)

      Nope -> do
        pure []

      FreshTemp -> do
        case temps of
          Temps [] -> error "run out of temps"
          Temps (z:zs) -> do
            let s' = s { temps = Temps zs }
            pure [([],zero,s',z)]

      FreshName -> do
        let (name,ss') = getFreshName ss
        let s' = s { ss = ss' }
        pure [([],zero,s',name)]

      FindOper oper -> do
        let x = findSemOper ss oper
        pure [([],zero,s,x)]

      FindName name -> do
        let regs = findSemState ss name
        pure [([],zero,s,regs)]

      Emit instruction semantics -> do
        let s' = s { ss = semantics ss }
        pure [ ([instruction], cost instruction, s', ()) ]

      Print mes -> do
        print mes
        pure [ ([],zero,s,()) ]


data State = State { ss :: SemState, temps :: Temps }

data Temps = Temps [ZeroPage]
