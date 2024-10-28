module Language
  ( Exp(..),Form(..),Op1(..), Op2(..), Var, Pred(..)
  , EvalEnv, eval
  , conv
  ) where

import Data.Bits (xor)
import Data.Map (Map)
import Data.Word (Word8)
import Text.Printf (printf)
import Util (look,extend)
import qualified Program as P

type Byte = Word8

----------------------------------------------------------------------
-- exp

data Exp = Form (Form Exp) | Var Var | Let Var Exp Exp | If Pred Exp Exp
  deriving (Eq)

data Pred = Equal Exp Exp
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
  Let x rhs body -> eval (extend ee x (eval ee rhs)) body
  If pred e1 e2 -> eval ee (if evalP ee pred then e1 else e2)
  Form form ->
    case form of
      Num n -> n
      Op1 Asl exp1 -> 2 * eval ee exp1
      Op2 Add exp1 exp2 -> eval ee exp1 + eval ee exp2
      Op2 Sub exp1 exp2 -> eval ee exp1 - eval ee exp2
      Op2 Xor exp1 exp2 -> eval ee exp1 `xor` eval ee exp2

evalP :: EvalEnv -> Pred -> Bool
evalP ee = \case
  Equal e1 e2 -> eval ee e1 == eval ee e2

----------------------------------------------------------------------
-- show

instance Show Exp where
  show = \case
    Let x rhs body -> printf "(let %s = %s in %s)" x (show rhs) (show body)
    If p e1 e2 -> printf "(if %s then %s else %s)" (show p) (show e1) (show e2)
    Form form -> show form
    Var x -> x

instance Show Pred where
  show = \case
    Equal e1 e2 -> printf "(%s == %s)" (show e1) (show e2)

instance Show a => Show (Form a) where
  show = \case
    Num n -> show n
    Op1 Asl e -> printf "(%s << 1)" (show e)
    Op2 Add e1 e2 -> printf "(%s + %s)" (show e1) (show e2)
    Op2 Sub e1 e2 -> printf "(%s - %s)" (show e1) (show e2)
    Op2 Xor e1 e2 -> printf "(%s ^ %s)" (show e1) (show e2)




----------------------------------------------------------------------
-- convert from old (Language) Exp into new Program, and compile that
-- and then we can kill the old Language code

conv :: Exp -> P.Exp
conv = \case
  Var x -> P.Var x
  Let x rhs body -> P.Let x (conv rhs) (conv body)
  If pred e1 e2 -> P.Ite (convP pred) (conv e1 ) (conv e2)
  Form form ->
    case form of
      Num n -> P.Num n
      Op1 Asl exp1 -> P.App "shl" [conv exp1]
      Op2 Add exp1 exp2 -> P.App "+" [conv exp1, conv exp2]
      Op2 Sub exp1 exp2 -> P.App "-" [conv exp1, conv exp2]
      Op2 Xor exp1 exp2 -> P.App "^" [conv exp1, conv exp2]

convP :: Pred -> P.Exp
convP = \case
  Equal e1 e2 -> P.App "==" [conv e1, conv e2]
