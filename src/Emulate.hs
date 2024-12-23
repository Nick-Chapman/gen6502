module Emulate
  ( MachineState(..), emulate,
  ) where

import Architecture (Reg(..),Flag(..),Immediate(..))
import Data.Bits ((.&.),xor)
import Data.Map (Map)
import Data.Word (Word8)
import Instruction (Code(..),Instruction(..),ITransfer(..),ICompute(..),ICompare(..))
import Util (look,extend)

type Byte = Word8

data MachineState = MS { regs :: Map Reg Byte, flags :: Map Flag Bool }

getReg :: MachineState -> Reg -> Byte
getReg MS{regs} reg = look "getReg" regs reg

emulate :: MachineState -> Code -> Reg -> Byte
emulate ms0 code locFinal = getReg (steps ms0 code) locFinal

steps :: MachineState -> Code -> MachineState
steps ms = \case
  Done -> ms
  Do i is -> steps (step ms i) is

step :: MachineState -> Instruction -> MachineState
step ms@MS{regs,flags} = do
  let get = getReg ms
  let testFlag = look "testFlag" flags

  let a = RegA
  let x = RegX
  let y = RegY

  let isN v = (v .&. 128) == 128

  let upNZ flags v = do ms { flags = extend (extend flags FlagN (isN v)) FlagZ (v==0) }
  let up k v = (upNZ flags v) { regs = extend regs k v }
  let store z = up (ZP z) -- dont update flags
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
        Sta z -> store z (get a)
        Stx z -> store z (get x)
        Sty z -> store z (get y)
    Compute i ->
      case i of
        -- TODO: adc/sbc should read/set carry flag
        Adci (Immediate b) -> up a (get a + b)
        Sbci (Immediate b) -> up a (get a - b)
        Andi (Immediate b) -> up a (get a .&. b)
        Eori (Immediate b) -> up a (get a `xor` b)
        Adcz z -> up a (get a + get (ZP z))
        Sbcz z -> up a (get a - get (ZP z))
        Andz z -> up a (get a .&. get (ZP z))
        Eorz z -> up a (get a `xor` get (ZP z))
        Inx -> up x (get x + 1)
        Iny -> up y (get y + 1)
        Asla -> up a (2 * get a)
        Lsra -> up a (get a `div` 2)
        -- These in-place mem operations DO update the flags
        Incz z -> up (ZP z) (get (ZP z) + 1)
        Aslz z -> up (ZP z) (2 * get (ZP z))
        Lsrz z -> up (ZP z) (get (ZP z) `div` 2)
    Compare i ->
      case i of
        Cmpz z -> upNZ flags (get a - get (ZP z))
        Cmpi (Immediate b) -> upNZ flags (get a - b)
    Branch flag code1 code2 ->
      steps ms (if testFlag flag then code1 else code2)
