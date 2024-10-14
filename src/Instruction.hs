module Instruction
  ( Code, Instruction(..), ITransfer(..), ICompute(..)
  , ZeroPage(..), Immediate(..)
  , Semantics, transfer, overwrite, noSemantics, transferSemantics, computeSemantics
  , SemState, Reg(..), Exp

  , initSS, getFreshName
  , makeSem, Name, Arg(..)

  , findSemState
  , findSemOper, Oper

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


-- TODO: move Semantics to own module
----------------------------------------------------------------------
-- Semantic values

newtype Name = NameU { unique :: Int }
  deriving (Eq)

instance Show Name where
  show NameU {unique} = printf "u%d" unique

data Arg = Imm Immediate | Name Name deriving (Eq)

instance Show Arg where
  show = \case
    Imm imm -> show imm
    Name name -> show name

type Oper = Form Arg

data Sem = Sem { name :: Name , operM :: Maybe Oper} deriving (Eq)

instance Show Sem where
  show Sem { name, operM } =
    printf "%s=%s" (show name)
    (case operM of
       Just x -> show x
       Nothing -> "*")

makeSem :: Name -> Maybe (Form Arg) -> Sem
makeSem name operM = Sem { name, operM }

----------------------------------------------------------------------
-- Semantic state (map from Reg)

data SemState =
  SS { names :: [Name] -- just an int would be simpler!
     , env :: Map Reg Sem
     }

instance Show SemState where
  show SS{ env } = show env

get :: Reg -> SemState -> Sem
get reg SS{env} = look "get" env reg

update :: Reg -> Sem -> SemState -> SemState
update reg sem ss@SS{env} = ss { env = extend env reg sem }

initSS :: [Reg] -> ([Name],SemState)
initSS xs =  do
  let (names1,names2) = splitAt (length xs) [ NameU { unique } | unique <- [1..] ]
  (names1,
   SS
    { names = names2
    , env = Map.fromList [ (reg,makeSem name Nothing) | (name,reg) <- zip names1 xs ]
    })

getFreshName :: SemState -> (Name,SemState)
getFreshName ss@SS{names} =
  case names of
    [] -> error "run out of names" -- not possible!
    name1:names -> do
      (name1, ss { names })

findSemState :: SemState -> Name -> [Reg]
findSemState SS{env} name1 =
  [ reg | (reg,Sem{name}) <- Map.toList env, name == name1 ]

findSemOper :: SemState -> Oper -> Maybe Name
findSemOper SS{env} oper1 =
  case [ name | (_,Sem{name,operM=Just oper}) <- Map.toList env, oper == oper1 ] of
    [] -> Nothing
    name:_ -> Just name

----------------------------------------------------------------------
-- Semantics (function over SemState)

type Semantics = SemState -> SemState

noSemantics :: Semantics
noSemantics = id

transfer :: Reg -> Reg -> Semantics
transfer src dest = \s -> update dest (get src s) s

overwrite :: Sem -> Reg -> Semantics
overwrite sem reg ss@SS{env} = do ss { env = extend env reg sem }

overwriteI :: Immediate -> Reg -> Semantics
overwriteI (Immediate byte) dest ss = do
  let (name,ss') = getFreshName ss
  let sem = Sem { name, operM = Just (Num byte) }
  update dest sem ss'

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
