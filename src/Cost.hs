module Cost
  ( Cost, lessSpace, lessTime, zero, add, cost,
  ) where

import Instruction (Instruction(..),ITransfer(..),ICompute(..))
import Text.Printf(printf)

data Cost = Cost { space :: Int, time :: Int }

instance Show Cost where
  show Cost{space,time} = printf "%d/%d" space time

lessSpace  :: Cost -> Cost -> Ordering
lessSpace (Cost {space=s1,time=t1}) (Cost {space=s2,time=t2}) =
  compare (s1,t1) (s2,t2)

lessTime  :: Cost -> Cost -> Ordering
lessTime (Cost {space=s1,time=t1}) (Cost {space=s2,time=t2}) =
  compare (t1,s1) (t2,s2)

zero :: Cost
zero = Cost { space = 0, time = 0 }

add :: Cost -> Cost -> Cost
add (Cost {space=s1,time=t1}) (Cost {space=s2,time=t2}) = Cost { space = s1+s2, time = t1+t2 }

cost :: Instruction -> Cost
cost i = Cost { space, time } where (space,time) = space_time i

space_time :: Instruction -> (Int,Int)
space_time = \case
  Clc -> (1,2)
  Tx i ->
    case i of
      Tax -> (1,2)
      Txa -> (1,2)
      Tya -> (1,2)
      Ldai{} -> (2,2)
      Ldxi{} -> (2,2)
      Ldaz{} -> (2,3)
      Ldxz{} -> (2,3)
      Sta{} -> (2,3)
      Stx{} -> (2,3)
      Sty{} -> (2,3)
  Comp i ->
    case i of
      Adci{} -> (2,3)
      Adcz{} -> (2,3)
      Inx -> (1,2)
      Incz{} -> (2,5)
      Asla -> (1,2)
