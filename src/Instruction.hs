module Instruction
  ( Code, Instruction(..), ITransfer(..), ICompute(..), ICompare(..)
  , transferSemantics, computeSemantics, compareSemantics
  ) where

import Semantics (Immediate(..),ZeroPage(..),Semantics,Sem,transfer,overwrite,overwriteI,noSemantics,Reg(..))
import Text.Printf (printf)

import Semantics (Sem1)

----------------------------------------------------------------------
-- instructions

type Code = [Instruction]

data Instruction = Tx ITransfer | Compute ICompute | Compare ICompare | Clc | Sec
  | Branch -- TODO need to know what instruction/condition we are branching on
    Code Code
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
  = Adcz ZeroPage | Adci Immediate
  | Sbcz ZeroPage | Sbci Immediate
  | Andz ZeroPage | Andi Immediate
  | Eorz ZeroPage | Eori Immediate

  | Inx | Iny
  | Incz ZeroPage
  | Asla
  | Aslz ZeroPage
  | Lsra
  | Lsrz ZeroPage
  deriving (Eq,Ord)

data ICompare
  = Cmpz ZeroPage | Cmpi Immediate
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
  Andi{} -> overwrite sem RegA
  Andz{} -> overwrite sem RegA
  Eori{} -> overwrite sem RegA
  Eorz{} -> overwrite sem RegA
  Inx -> overwrite sem RegX
  Iny -> overwrite sem RegY
  Incz z -> overwrite sem (ZP z)
  Asla -> overwrite sem RegA
  Aslz z -> overwrite sem (ZP z)
  Lsra -> overwrite sem RegA
  Lsrz z -> overwrite sem (ZP z)

compareSemantics :: Sem1 -> ICompare -> Semantics
compareSemantics _sem1 = \case -- TODO
  Cmpz{} -> noSemantics -- undefined sem1
  Cmpi{} -> noSemantics --undefined

----------------------------------------------------------------------
-- show instruction

instance Show Instruction where
  show = \case
    Tx i -> show i
    Compute i -> show i
    Compare i -> show i
    Clc -> "clc"
    Sec -> "sec"
    Branch xs1 xs2 -> printf "b? %s %s" (show xs1) (show xs2)

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
    Lsra -> "lsr a"
    Aslz a -> oneArg "asl" a
    Lsrz a -> oneArg "lsr" a
    Adci a -> oneArg "adc" a
    Sbcz a -> oneArg "sbc" a
    Sbci a -> oneArg "sbc" a
    Adcz a -> oneArg "adc" a
    Andi a -> oneArg "and" a
    Andz a -> oneArg "and" a
    Eori a -> oneArg "eor" a
    Eorz a -> oneArg "eor" a
    Incz a -> oneArg "inc" a

instance Show ICompare where
  show = \case
    Cmpi a -> oneArg "cmp" a
    Cmpz a -> oneArg "cmp" a


oneArg :: Show a => String -> a -> String
oneArg name x = printf "%s %s" name (show x)
