module Emulate
  ( MachineState, initMS, emulate, EmuEnv
  ) where

import Data.Bits (xor)
import Data.Map (Map)
import Data.Word (Word8)
import Instruction (Code,Instruction(..),ITransfer(..),ICompute(..),ICompare(..))
import Language (Var,EvalEnv)
import Semantics (Reg(..),Immediate(..))
import Util (look,extend)
import qualified Data.Map as Map

type Byte = Word8

type EmuEnv = Map Var Reg

emulate :: MachineState -> Code -> Reg -> Byte
emulate ms0 code locFinal = get_loc (steps ms0 code) locFinal

----------------------------------------------------------------------
-- machine state

data MachineState = MS { m :: Map Reg Byte } -- flags will go in here also
  deriving Show

initMS :: EmuEnv -> EvalEnv -> MachineState
initMS env ee = do
  let m = Map.fromList [ (loc,look "initMS" ee var) | (var,loc) <- Map.toList env ]
  MS m

get_loc :: MachineState -> Reg -> Byte
get_loc MS{m} loc = look "get_loc" m loc
--get_loc MS{m} loc = _look1 42 m loc -- hack

_look1 :: (Ord k, Show k) => v -> Map k v -> k -> v
_look1 def m k = maybe def id (Map.lookup k m)


----------------------------------------------------------------------
-- step instruction

steps :: MachineState -> Code -> MachineState
steps ms = \case
  [] -> ms
  i:is -> steps (step ms i) is

step :: MachineState -> Instruction -> MachineState
step ms@MS{m} = do
  let get loc = get_loc ms loc
  let a = RegA
  let x = RegX
  let y = RegY
  let up k v = ms { m = extend m k v }
  \case
    Clc -> ms
    Sec -> ms
    Tx i ->
      case i of
        Tax -> up x (get a)
        Tay -> up y (get a)
        Txa -> up a (get x)
        Tya -> up a (get y)
        Ldai (Immediate b) -> up a b
        Ldxi (Immediate b) -> up x b
        Ldyi (Immediate b) -> up y b
        Ldaz z -> up a (get (ZP z))
        Ldxz z -> up x (get (ZP z))
        Ldyz z -> up y (get (ZP z))
        Sta z -> up (ZP z) (get a)
        Stx z -> up (ZP z) (get x)
        Sty z -> up (ZP z) (get y)
    Compute i ->
      case i of
        -- TODO: adc/sbc should read/set carry flag
        Adci (Immediate b) -> up a (get a + b)
        Adcz z -> up a (get a + get (ZP z))
        Sbci (Immediate b) -> up a (get a - b)
        Sbcz z -> up a (get a - get (ZP z))
        Eori (Immediate b) -> up a (get a `xor` b)
        Eorz z -> up a (get a `xor` get (ZP z))
        Inx -> up x (get x + 1)
        Iny -> up y (get y + 1)
        Incz z -> up (ZP z) (get (ZP z) + 1)
        Asla -> up a (2 * get a)
        Aslz z -> up (ZP z) (2 * get (ZP z))
    Compare i ->
      case i of
        Cmpz{} -> ms -- TODO: need to track flag values
        Cmpi{} -> ms
    Branch _code1 code2 ->
      -- TODO need to look at flag value to see where to go
      -- for now lets big 2nd alternative
      steps ms code2
