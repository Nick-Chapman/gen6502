module Asm
  ( Asm(..), runAsm, Temps(..), Cost
  ) where

import Control.Monad (ap,liftM)
import Cost(Cost,cost)
import Data.List (sortBy)
import Instruction (Code,Instruction,ZeroPage,Semantics,SemState)
import Text.Printf (printf)
import qualified Cost

instance Functor Asm where fmap = liftM
instance Applicative Asm where pure = Ret; (<*>) = ap
instance Monad Asm where (>>=) = Bind

data Asm a where
  Ret :: a -> Asm a
  Bind :: Asm a -> (a -> Asm b) -> Asm b
  Alt :: String -> Asm a -> Asm a -> Asm a
  Nope :: String -> Asm a
  GetSemState :: Asm SemState
  SetSemState :: SemState -> Asm ()
  Fresh :: Asm ZeroPage
  Emit :: Instruction -> Semantics -> Asm ()

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
      then sortBy (\(_,c1,_) (_,c2,_) -> costOrdering c1 c2)
      else id

    s0 = State { ss = ss0, temps = temps0 }

    _x :: String = printf ""
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

      Alt _tag m1 m2 -> do
        --printf "Alt: %s\n" _tag
        rs1 <- loop s m1
        rs2 <- loop s m2
        pure (rs1 ++ rs2)

      Nope _tag -> do
        --printf "--Nope[%s]\n" _tag
        pure []

      GetSemState -> do
        pure [([],zero,s,ss)]

      SetSemState ss -> do
        let s' = s { ss }
        pure [([],zero,s',())]

      Fresh -> do
        case temps of
          Temps [] -> error "run out of temps"
          Temps (z:zs) -> do
            let s' = s { temps = Temps zs }
            pure [([],zero,s',z)]

      Emit instruction semantics -> do
        let s' = s { ss = semantics ss }
        pure [ ([instruction], cost instruction, s', ()) ]


data State = State { ss :: SemState, temps :: Temps }

data Temps = Temps [ZeroPage]
