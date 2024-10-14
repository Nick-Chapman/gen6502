module Language
  ( Exp(..),Form(..),Op1(..), Op2(..), Var
  , EvalEnv, eval
  ) where

import Data.Bits (xor)
import Data.Map (Map)
import Data.Word (Word8)
import Text.Printf (printf)
import Util (look)

type Byte = Word8

----------------------------------------------------------------------
-- exp

data Exp = Form (Form Exp) | Var Var
  deriving (Eq)

data Form e = Num Byte | Op2 Op2 e e | Op1 Op1 e
  deriving (Eq)

data Op1 = Asl
  deriving (Eq)

data Op2 = Add | Sub | Xor
  deriving (Eq)

type Var = String

----------------------------------------------------------------------
-- eval

type EvalEnv = Map Var Byte

eval :: EvalEnv -> Exp -> Byte
eval ee = \case
  Var x -> look "eval" ee x
  Form form ->
    case form of
      Num n -> n
      Op1 Asl exp1 -> 2 * eval ee exp1
      Op2 Add exp1 exp2 -> eval ee exp1 + eval ee exp2
      Op2 Sub exp1 exp2 -> eval ee exp1 - eval ee exp2
      Op2 Xor exp1 exp2 -> eval ee exp1 `xor` eval ee exp2

----------------------------------------------------------------------
-- show

instance Show Exp where
  show = \case
    Form form -> show form
    Var x -> x

instance Show a => Show (Form a) where
  show = \case
    Num n -> show n
    Op1 Asl e -> printf "(%s << 1)" (show e)
    Op2 Add e1 e2 -> printf "(%s + %s)" (show e1) (show e2)
    Op2 Sub e1 e2 -> printf "(%s - %s)" (show e1) (show e2)
    Op2 Xor e1 e2 -> printf "(%s ^ %s)" (show e1) (show e2)
