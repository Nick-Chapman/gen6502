module Instruction
  ( Code, Instruction(..), ITransfer(..), ICompute(..)
  , transferSemantics, computeSemantics
  ) where

import Semantics (Immediate(..),ZeroPage(..),Semantics,Sem,transfer,overwrite,overwriteI,Reg(..))
import Text.Printf (printf)

----------------------------------------------------------------------
-- instructions

type Code = [Instruction]

data Instruction = Tx ITransfer | Comp ICompute | Clc | Sec
  deriving (Eq,Ord)

data ITransfer
  = Tax | Tay | Txa | Tya
  | Ldai Immediate | Ldaz ZeroPage
  | Ldxi Immediate | Ldxz ZeroPage
  | Ldyi Immediate | Ldyz ZeroPage
  | Sta ZeroPage
  | Stx ZeroPage
  | Sty ZeroPage
  deriving (Eq,Ord)

data ICompute
  = Adcz ZeroPage
  | Adci Immediate
  | Sbcz ZeroPage
  | Sbci Immediate
  | Eorz ZeroPage
  | Eori Immediate
  | Inx | Iny
  | Incz ZeroPage
  | Asla
  | Aslz ZeroPage
  deriving (Eq,Ord)

----------------------------------------------------------------------
-- instruction semantics

transferSemantics :: ITransfer -> Semantics
transferSemantics = \case
  Tax -> transfer RegA RegX
  Tay -> transfer RegA RegY
  Txa -> transfer RegX RegA
  Tya -> transfer RegY RegA
  Ldai i -> overwriteI i RegA
  Ldxi i -> overwriteI i RegX
  Ldyi i -> overwriteI i RegY
  Ldaz z -> transfer (ZP z) RegA
  Ldxz z -> transfer (ZP z) RegX
  Ldyz z -> transfer (ZP z) RegY
  Sta z -> transfer RegA (ZP z)
  Stx z -> transfer RegX (ZP z)
  Sty z -> transfer RegY (ZP z)

computeSemantics :: Sem -> ICompute -> Semantics
computeSemantics sem = \case
  Adci{} -> overwrite sem RegA
  Adcz{} -> overwrite sem RegA
  Sbci{} -> overwrite sem RegA
  Sbcz{} -> overwrite sem RegA
  Eori{} -> overwrite sem RegA
  Eorz{} -> overwrite sem RegA
  Inx -> overwrite sem RegX
  Iny -> overwrite sem RegY
  Incz z -> overwrite sem (ZP z)
  Asla -> overwrite sem RegA
  Aslz z -> overwrite sem (ZP z)

----------------------------------------------------------------------
-- show instruction

instance Show Instruction where
  show = \case
    Tx i -> show i
    Comp i -> show i
    Clc -> "clc"
    Sec -> "sec"

instance Show ITransfer where
  show = \case
    Tax -> "tax"
    Tay -> "tay"
    Txa -> "txa"
    Tya -> "tya"
    Ldai a -> oneArg "lda" a
    Ldaz a -> oneArg "lda" a
    Ldxi a -> oneArg "ldx" a
    Ldxz a -> oneArg "ldx" a
    Ldyi a -> oneArg "ldy" a
    Ldyz a -> oneArg "ldy" a
    Sta a -> oneArg "sta" a
    Stx a -> oneArg "stx" a
    Sty a -> oneArg "sty" a

instance Show ICompute where
  show = \case
    Inx -> "inx"
    Iny -> "iny"
    Asla -> "asl a"
    Aslz a -> oneArg "asl" a
    Adci a -> oneArg "adc" a
    Sbcz a -> oneArg "sbc" a
    Sbci a -> oneArg "sbc" a
    Adcz a -> oneArg "adc" a
    Eori a -> oneArg "eor" a
    Eorz a -> oneArg "eor" a
    Incz a -> oneArg "inc" a

oneArg :: Show a => String -> a -> String
oneArg name x = printf "%s %s" name (show x)
