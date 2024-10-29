module SemState
  ( Name(..), Arg(..), Arg1(..)
  , SemState, initSS, lookSS, updateSS
  , findSemState, lookupReg
  ) where

import Architecture (Immediate,Reg(..))
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

initSS :: Map Reg Arg -> SemState
initSS env = SS { env }

lookSS :: String -> Reg -> SemState -> Arg
lookSS tag reg SS{env} = look (printf "lookSS(%s)" tag) env reg

updateSS :: Reg -> Arg -> SemState -> SemState
updateSS reg arg ss@SS{env} = ss { env = extend env reg arg }

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
