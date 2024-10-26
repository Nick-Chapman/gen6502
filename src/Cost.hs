module Cost
  ( Cost, lessSpace, lessTime, zero, add, cost, costOfCode
  ) where

import Instruction (Code(),Instruction(..),ITransfer(..),ICompute(..),ICompare(..))
import Text.Printf(printf)

data Cost = Cost { space :: Int, time :: Int } deriving Eq

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

costOfCode :: Code -> Cost
costOfCode = foldl Cost.add Cost.zero . map cost

cost :: Instruction -> Cost
cost i = Cost { space, time } where (space,time) = space_time i

space_time :: Instruction -> (Int,Int)
space_time = \case
  Clc -> (1,2)
  Sec -> (1,2)
  Tx i ->
    case i of
      Tax -> (1,2)
      Tay -> (1,2)
      Txa -> (1,2)
      Tya -> (1,2)
      Ldai{} -> (2,2)
      Ldxi{} -> (2,2)
      Ldyi{} -> (2,2)
      Ldaz{} -> (2,3)
      Ldxz{} -> (2,3)
      Ldyz{} -> (2,3)
      Sta{} -> (2,3)
      Stx{} -> (2,3)
      Sty{} -> (2,3)
  Compute i ->
    case i of
      Adci{} -> (2,2)
      Sbci{} -> (2,2)
      Andi{} -> (2,2)
      Eori{} -> (2,2)
      Adcz{} -> (2,3)
      Sbcz{} -> (2,3)
      Andz{} -> (2,3)
      Eorz{} -> (2,3)
      Inx -> (1,2)
      Iny -> (1,2)
      Incz{} -> (2,5)
      Asla -> (1,2)
      Lsra -> (1,2)
      Aslz{} -> (2,5)
      Lsrz{} -> (2,5)
  Compare i ->
    case i of
      Cmpi{} -> (2,2)
      Cmpz{} -> (2,3)
  Branch code1 code2 -> do
    let Cost{space=space1,time=time1} = costOfCode code1
    let Cost{space=space2,time=time2} = costOfCode code2
    let maxTime = max time1 time2 -- TODO track max and min & allow ordering to chosse
    (2+space1+space2, maxTime)
