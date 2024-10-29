
-- TODO: Is Semantics really the write name for the tracking of Names to regs?
module Semantics
  ( Immediate(..), ZeroPage(..), Reg(..), Flag(..)

  , Semantics, noSemantics, transfer, overwrite, overwriteI

  , SemState, initSS

  , getFreshName
  , findSemState
  , lookupReg

  -- enum of operations which compute 8bit and 1bit values
  -- TODO: move these out of here
  , Oper(..), Pred(..)

  , Name, Arg(..), Arg1(..)

  ) where

import Data.Map (Map)
import Data.Word (Word8)
import Text.Printf (printf)
import Util (look,extend)
import qualified Data.Map as Map

type Byte = Word8

data Arg1 = Name1 Name -- = Yes | No | Name1 Name1
  deriving (Eq,Ord)

data Pred = Equal Arg Arg
  deriving (Eq,Show)


----------------------------------------------------------------------
-- Semantic values

newtype Name = NameU { unique :: Int }
  deriving (Eq,Ord)

instance Show Name where
  show NameU {unique} = printf "u%d" unique

data Arg = Imm Immediate | Name Name deriving (Eq,Ord)

instance Show Arg where
  show = \case
    Imm imm -> show imm
    Name name -> show name

-- TODO: Oper move out of here once Semantics tracks just which Names are in each register
data Oper
  = ONum Byte
  | Add Arg Arg
  | Sub Arg Arg
  | And Arg Arg
  | Eor Arg Arg
  | Asl Arg
  | Lsr Arg
  -- TODO: note. adding a new Semantic form here causes no compile time errors.
  -- because of the way Cogen is setup. This is not really a good thing!
  deriving (Eq,Show)

----------------------------------------------------------------------
-- Semantic state (map from Reg)

-- TODO: maybe moves Names out of SS and just thread in Asm monad
-- TODO: rename SS ?
data SemState =
  SS { names :: [Name] -- just an int would be simpler! -- TODO: do it the simpler way
     , env :: Map Reg Name -- TODO: map to Arg not just Name
     }

instance Show SemState where
  show SS{ env } = show env

getRegInSS :: String -> Reg -> SemState -> Name
getRegInSS tag reg SS{env} = look (printf "getRegInSS(%s)" tag) env reg

update :: Reg -> Name -> SemState -> SemState
update reg name ss@SS{env} = ss { env = extend env reg name }

initSS :: [Reg] -> ([Name],SemState)
initSS xs =  do
  let (names1,names2) = splitAt (length xs) [ NameU { unique } | unique <- [1..] ]
  (names1,
   SS
    { names = names2
    , env = Map.fromList [ (reg,name) | (name,reg) <- zip names1 xs ]
    })

-- TODO: move fresh name handling to Asm
getFreshName :: SemState -> (Name,SemState)
getFreshName ss@SS{names} =
  case names of
    [] -> error "run out of names" -- not possible!
    name1:names -> do
      (name1, ss { names })

-- find all the places a name is located...
findSemState :: SemState -> Name -> [Reg]
findSemState SS{env} nameK =
  [ reg | (reg,name) <- Map.toList env, name == nameK ]

-- find if a name is located in a specific register...
lookupReg :: SemState -> Reg -> Maybe Name
lookupReg SS{env} reg =
  case Map.lookup reg env of
    Nothing -> Nothing
    Just name -> Just name

----------------------------------------------------------------------
-- Semantics (function over SemState)

type Semantics = SemState -> SemState

noSemantics :: Semantics
noSemantics = id

transfer :: Reg -> Reg -> Semantics
transfer src dest = \s -> update dest (getRegInSS tag src s) s
  where tag = printf "transfer:%s-->%s" (show src) (show dest)

overwrite :: Name -> Reg -> Semantics
overwrite name reg ss@SS{env} = do ss { env = extend env reg name }

overwriteI :: Immediate -> Reg -> Semantics
overwriteI (Immediate _IGNORED_byte) dest ss = do -- TODO: byte is ignored
  let (name,ss') = getFreshName ss
  update dest name ss'

----------------------------------------------------------------------
-- Reg/Flag

data Reg = RegA | RegX | RegY | ZP ZeroPage
  deriving (Eq,Ord)

-- 1bit equivalent of Reg
data Flag = FlagZ -- TODO: need flags for N/C
  deriving (Eq,Ord)

instance Show Reg where
  show = \case
    RegA -> "A"
    RegX -> "X"
    RegY -> "Y"
    ZP z -> "ZP-" ++ show z

instance Show Flag where
  show = \case
    FlagZ -> "z"

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
