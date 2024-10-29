
module Semantics
  ( Name(..), Arg(..), Arg1(..)
  , SemState, initSS, findSemState, lookupReg
  , Semantics , noSemantics, transferSemantics, computeSemantics, compareSemantics
  ) where

import Architecture (Immediate,Reg(..))
import Data.Map (Map)
import Instruction
import Text.Printf (printf)
import Util (look,extend)
import qualified Data.Map as Map

----------------------------------------------------------------------
-- Semantic values -- Name,Arg

newtype Name = NameU { unique :: Int }
  deriving (Eq,Ord)

instance Show Name where
  show NameU {unique} = printf "u%d" unique

data Arg = Imm Immediate | Name Name deriving (Eq,Ord)

instance Show Arg where
  show = \case
    Imm imm -> show imm
    Name name -> show name

data Arg1 = Name1 Name -- = Yes | No | Name1 Name1
  deriving (Eq,Ord)

----------------------------------------------------------------------
-- Semantic state (map from Reg)

-- TODO: rename SS ?
-- TODO: example that knows a reg has an immediate in it
data SemState = SS {env :: Map Reg Arg}

getRegInSS :: String -> Reg -> SemState -> Arg
getRegInSS tag reg SS{env} = look (printf "getRegInSS(%s)" tag) env reg

update :: Reg -> Arg -> SemState -> SemState
update reg arg ss@SS{env} = ss { env = extend env reg arg }

initSS :: Map Reg Arg -> SemState
initSS env = SS { env }

-- find all the places a name is located...
findSemState :: SemState -> Name -> [Reg]
findSemState SS{env} nameK =
  [ reg | (reg,Name name) <- Map.toList env, name == nameK ]

-- find if a name is located in a specific register...
lookupReg :: SemState -> Reg -> Maybe Name
lookupReg SS{env} reg =
  case Map.lookup reg env of
    Nothing -> Nothing
    Just Imm{} -> Nothing
    Just (Name name) -> Just name

----------------------------------------------------------------------
-- Semantics (function over SemState)

type Semantics = SemState -> SemState

noSemantics :: Semantics
noSemantics = id

transfer :: Reg -> Reg -> Semantics
transfer src dest = \s -> update dest (getRegInSS tag src s) s
  where tag = printf "transfer:%s-->%s" (show src) (show dest)

overwrite :: Name -> Reg -> Semantics
overwrite name reg ss@SS{env} = do ss { env = extend env reg (Name name) }

overwriteI :: Immediate -> Reg -> Semantics
overwriteI imm reg ss@SS{env}  = ss { env = extend env reg (Imm imm) }


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

-- TODO: These should return Asm Name, doing the freshName as required
-- passing back the name, rather than taking the Sem(=Name)
computeSemantics :: Name -> ICompute -> Semantics
computeSemantics name = \case
  Adci{} -> overwrite name RegA
  Adcz{} -> overwrite name RegA
  Sbci{} -> overwrite name RegA
  Sbcz{} -> overwrite name RegA
  Andi{} -> overwrite name RegA
  Andz{} -> overwrite name RegA
  Eori{} -> overwrite name RegA
  Eorz{} -> overwrite name RegA
  Inx -> overwrite name RegX
  Iny -> overwrite name RegY
  Incz z -> overwrite name (ZP z)
  Asla -> overwrite name RegA
  Aslz z -> overwrite name (ZP z)
  Lsra -> overwrite name RegA
  Lsrz z -> overwrite name (ZP z)

compareSemantics :: Name -> ICompare -> Semantics
compareSemantics _name = \case -- TODO
  Cmpz{} -> noSemantics -- undefined sem1
  Cmpi{} -> noSemantics --undefined
