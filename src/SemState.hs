module SemState
  ( Name(..), Arg(..), Name1(..), Arg1(..)
  , SemState, initSS, lookSS, updateSS, updateSS1
  , lookupReg
  , lookupName, lookupName1
  ) where

import Architecture (Immediate,Reg(..),Flag(..))
import Data.Map (Map)
import Text.Printf (printf)
import Util (look,extend)
import qualified Data.Map as Map

----------------------------------------------------------------------
-- Semantic values -- Name,Arg

newtype Name = Name8U { unique :: Int }
  deriving (Eq,Ord)

newtype Name1 = Name1U { unique :: Int }
  deriving (Eq,Ord)

instance Show Name where
  show Name8U {unique} = printf "b%d" unique

instance Show Name1 where
  show Name1U {unique} = printf "c%d" unique

data Arg = Imm Immediate | Name Name deriving (Eq,Ord)

data Arg1 = Name1 Name1 -- = Yes | No | Name1 Name1
  deriving (Eq,Ord)

instance Show Arg where
  show = \case
    Imm imm -> show imm
    Name name -> show name

----------------------------------------------------------------------
-- Semantic state (map from Reg)

-- TODO: example that knows a reg has an immediate in it
data SemState = SS
  { env8 :: Map Reg Arg
  , env1 :: Map Flag Arg1
  }

initSS :: Map Reg Arg -> SemState
initSS env8 = SS { env8, env1 = Map.empty }

lookSS :: String -> Reg -> SemState -> Arg
lookSS tag reg SS{env8} = look (printf "lookSS(%s)" tag) env8 reg

updateSS :: Reg -> Arg -> SemState -> SemState
updateSS reg arg ss@SS{env8} = ss { env8 = extend env8 reg arg }

updateSS1 :: Flag -> Arg1 -> SemState -> SemState
updateSS1 flag arg ss@SS{env1} = ss { env1 = extend env1 flag arg }


-- find if a name is located in a specific register...
lookupReg :: SemState -> Reg -> Maybe Name
lookupReg SS{env8} reg =
  case Map.lookup reg env8 of
    Nothing -> Nothing
    Just Imm{} -> Nothing
    Just (Name name) -> Just name

-- find all the (reg)places a name is located...
lookupName :: Name -> SemState -> [Reg]
lookupName sought SS{env8} =
  [ reg | (reg,Name name) <- Map.toList env8, name == sought ]

-- find all the (flag)places a name1 is located...
lookupName1 :: Name1 -> SemState -> [Flag]
lookupName1 sought SS{env1} =
  [ reg | (reg,Name1 name1) <- Map.toList env1, name1 == sought ]
