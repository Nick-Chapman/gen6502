module Instruction
  ( Code, Instruction(..), ITransfer(..), ICompute(..)
  , ZeroPage(..), Immediate(..)
  , Semantics, transfer, overwrite, noSemantics, transferSemantics, computeSemantics
  , SemState, Reg(..), Exp, makeSemState, findSemState
  ) where

import Util (look,extend)
import Language (Exp(..),Form(..))
import Data.Word (Word8)
import Text.Printf (printf)
import Data.Map (Map)
import qualified Data.Map as Map

type Byte = Word8

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

computeSemantics :: Exp -> ICompute -> Semantics
computeSemantics e = \case
  Adci{} -> overwrite e RegA
  Adcz{} -> overwrite e RegA
  Sbci{} -> overwrite e RegA
  Sbcz{} -> overwrite e RegA
  Eori{} -> overwrite e RegA
  Eorz{} -> overwrite e RegA
  Inx -> overwrite e RegX
  Iny -> overwrite e RegY
  Incz z -> overwrite e (ZP z)
  Asla -> overwrite e RegA
  Aslz z -> overwrite e (ZP z)

----------------------------------------------------------------------
-- Semantics

newtype SemState = SS { env :: Map Reg Exp }

makeSemState :: Map Reg Exp -> SemState
makeSemState env = SS { env = Map.fromList [ (loc,e) | (loc,e) <- Map.toList env ] }

findSemState :: SemState -> Exp -> [Reg]
findSemState SS{env} exp = [ loc | (loc,exp1) <- Map.toList env, exp == exp1 ]

type Semantics = SemState -> SemState

noSemantics :: Semantics
noSemantics = id

get :: Reg -> SemState -> Exp
get loc SS{env} = look "get" env loc

transfer :: Reg -> Reg -> Semantics
transfer src dest = \s -> overwrite (get src s) dest s

overwrite :: Exp -> Reg -> Semantics
overwrite exp reg SS{env} = SS { env = extend env reg exp }

overwriteI :: Immediate -> Reg -> Semantics
overwriteI (Immediate byte) reg = overwrite (Exp (Num byte)) reg

----------------------------------------------------------------------
-- Reg

data Reg = RegA | RegX | RegY | ZP ZeroPage
  deriving (Eq,Ord)

instance Show Reg where
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
