module Instruction
  ( Code, Instruction(..), ITransfer(..), ICompute(..)
  , ZeroPage(..), Immediate(..)
  , Semantics, transfer, overwrite, noSemantics, transferSemantics, computeSemantics
  , SemState, Loc(..)
  ) where

import Util (look,extend)
import Language (Exp(..),Form(..))
import Data.Word (Word8)
import Text.Printf (printf)
import Data.Map (Map)

type Byte = Word8

----------------------------------------------------------------------
-- instructions

type Code = [Instruction]

data Instruction = Tx ITransfer | Comp ICompute | Clc
  deriving Eq

data ITransfer
  = Tax | Tay | Txa | Tya
  | Ldai Immediate | Ldaz ZeroPage
  | Ldxi Immediate | Ldxz ZeroPage
  | Ldyi Immediate | Ldyz ZeroPage
  | Sta ZeroPage
  | Stx ZeroPage
  | Sty ZeroPage
  deriving Eq

data ICompute
  = Adcz ZeroPage
  | Adci Immediate
  | Eorz ZeroPage
  | Eori Immediate
  | Inx | Iny
  | Incz ZeroPage
  | Asla
  | Aslz ZeroPage
  deriving Eq

----------------------------------------------------------------------
-- instruction semantics

transferSemantics :: ITransfer -> Semantics
transferSemantics = \case
  Tax -> transfer RegA RegX
  Tay -> transfer RegA RegY
  Txa -> transfer RegX RegA
  Tya -> transfer RegY RegA
  Ldai (Immediate b) -> overwrite (Exp (Num b)) RegA
  Ldxi (Immediate b) -> overwrite (Exp (Num b)) RegX
  Ldyi (Immediate b) -> overwrite (Exp (Num b)) RegY
  Ldaz z -> transfer (ZP z) RegA
  Ldxz z -> transfer (ZP z) RegX
  Ldyz z -> transfer (ZP z) RegY
  Sta z -> transfer RegA (ZP z)
  Stx z -> transfer RegX (ZP z)
  Sty z -> transfer RegY (ZP z)

computeSemantics :: Exp -> ICompute -> Semantics
computeSemantics e = \case
  Adci{} -> overwrite e RegA
  Adcz{} -> overwrite e RegA
  Eori{} -> overwrite e RegA
  Eorz{} -> overwrite e RegA
  Inx -> overwrite e RegX
  Iny -> overwrite e RegY
  Incz z -> overwrite e (ZP z)
  Asla -> overwrite e RegA
  Aslz z -> overwrite e (ZP z)

----------------------------------------------------------------------
-- Semantics

type Semantics = SemState -> SemState

noSemantics :: Semantics
noSemantics = id

transfer :: Loc -> Loc -> Semantics
transfer src dest = \s -> extend s dest (getS src s)

overwrite :: Exp -> Loc -> Semantics
overwrite e loc s = extend s loc [e]

getS :: Loc -> SemState -> [Exp]
getS loc s = look "getS" s loc

type SemState = Map Loc [Exp]

----------------------------------------------------------------------
-- Loc

data Loc = RegA | RegX | RegY | ZP ZeroPage
  deriving (Eq,Ord)

instance Show Loc where
  show = \case
    RegA -> "A"
    RegX -> "X"
    RegY -> "Y"
    ZP z -> "ZP-" ++ show z

----------------------------------------------------------------------
-- show instruction

instance Show Instruction where
  show = \case
    Tx i -> show i
    Comp i -> show i
    Clc -> "clc"

instance Show ITransfer where
  show = \case
    Tax -> "tax"
    Tay -> "tay"
    Txa -> "txa"
    Tya -> "tya"
    -- TODO: show one-arg print code
    Ldai v -> printf "lda %s" (show v)
    Ldaz z -> printf "lda %s" (show z)
    Ldxi v -> printf "ldx %s" (show v)
    Ldxz z -> printf "ldx %s" (show z)
    Ldyi v -> printf "ldy %s" (show v)
    Ldyz z -> printf "ldy %s" (show z)
    Sta z -> printf "sta %s" (show z)
    Stx z -> printf "stx %s" (show z)
    Sty z -> printf "sty %s" (show z)

instance Show ICompute where
  show = \case
    Adci v -> printf "adc %s" (show v)
    Adcz z -> printf "adc %s" (show z)
    Eori v -> printf "eor %s" (show v)
    Eorz z -> printf "eor %s" (show z)
    Inx -> "inx"
    Iny -> "iny"
    Incz z -> printf "inc %s" (show z)
    Asla -> "asl a"
    Aslz z -> printf "asl %s" (show z)

----------------------------------------------------------------------
-- ZeroPage & Immediate

newtype ZeroPage = ZeroPage Byte
  deriving (Eq,Ord,Num)

newtype Immediate = Immediate Byte
  deriving (Eq,Ord,Num)

instance Show ZeroPage where
  show (ZeroPage z) = show z

instance Show Immediate where
  show (Immediate b) = "#" ++ show b

