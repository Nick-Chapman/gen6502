
module Semantics
  ( Immediate(..), ZeroPage(..), Reg(..)
  , Name, Arg(..), Oper, Sem, makeSem
  , Semantics, noSemantics, transfer, overwrite, overwriteI
  , SemState, initSS, getFreshName, findSemState, findSemOper
  ) where

import Data.Map (Map)
import Data.Word (Word8)
import Language (Form(..),Op2(..))
import Text.Printf (printf)
import Util (look,extend)
import qualified Data.Map as Map

type Byte = Word8

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

type Oper = Form Arg

data Sem = Sem { name :: Name , operM :: Maybe Oper} deriving (Eq)

instance Show Sem where
  show Sem { name, operM } =
    printf "%s=%s" (show name)
    (case operM of
       Just x -> show x
       Nothing -> "*")

makeSem :: Name -> Form Arg -> Sem
makeSem name oper = Sem { name, operM = Just (canonicaliseForm oper) }


canonicaliseForm :: Form Arg -> Form Arg -- TODO: not here
canonicaliseForm = \case
  Num n -> Num n
  Op1 op a1 -> Op1 op a1
  Op2 op@Sub a1 a2 -> Op2 op a1 a2
  Op2 op@Add a1 a2 -> Op2 op a1' a2' where (a1',a2') = order (a1,a2)
  Op2 op@Xor a1 a2 -> Op2 op a1' a2' where (a1',a2') = order (a1,a2)

  where order (a1,a2) = if a1 < a2 then (a1,a2) else (a2,a1)

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
    , env = Map.fromList [ (reg,Sem { name, operM = Nothing}) | (name,reg) <- zip names1 xs ]
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
findSemOper SS{env} oper1 = do
  let operK = canonicaliseForm oper1 -- TODO no
  case [ name | (_,Sem{name,operM=Just oper}) <- Map.toList env, oper == operK ] of
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
-- ZeroPage & Immediate

newtype ZeroPage = ZeroPage Byte
  deriving (Eq,Ord,Num)

newtype Immediate = Immediate Byte
  deriving (Eq,Ord,Num)

instance Show ZeroPage where
  show (ZeroPage z) = show z

instance Show Immediate where
  show (Immediate b) = "#" ++ show b
