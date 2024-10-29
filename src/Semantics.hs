
module Semantics
  ( Name(..), Arg(..), Arg1(..)
  , SemState, initSS, findSemState, lookupReg
  , Semantics, noSemantics, transfer, overwrite, overwriteI
  ) where

import Architecture (Immediate,Reg)
import Data.Map (Map)
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
