module Compile
  ( compileTarget,Env
  ) where

import Asm (Asm(..))
import Codegen (preamble,codegen,assign,Reg,Arg)
import Semantics (Arg1)
import Data.Map (Map)
import Language (Exp(..),Form(..),Op2(..),Op1(..),Var,Pred(..))
import Util (look,extend)
import qualified Semantics as S

import Codegen (codegenPred,codegenBranch)

type Env = Map Var Arg

compileTarget :: Env -> Exp -> Reg -> Asm ()
compileTarget env exp reg = do
  preamble
  arg <- compile env exp
  assign reg arg


compileP :: Env -> Pred -> Asm Arg1
compileP env = \case
  Equal exp1 exp2 -> do
    arg1 <- compile env exp1
    arg2 <- compile env exp2
    codegenPred (S.Equal arg1 arg2)

compile :: Env -> Exp -> Asm Arg
compile env exp = do
  case exp of
    Var x -> pure (look "compile" env x)

    Let x rhs body -> do
      rhs <- compile env rhs
      compile (extend env x rhs) body

    If p e1 e2 -> do
      p <- compileP env p
      p <- codegenBranch p
      Branch p (compile env e1) (compile env e2)

    Form form -> case form of

      Num n -> codegen (S.Num n)

      Op1 op1 exp1 -> do
        arg1 <- compile env exp1
        codegen (convOp1 op1 arg1)

      Op2 op2 exp1 exp2 -> do
        arg1 <- compile env exp1
        arg2 <- compile env exp2
        codegen (convOp2 op2 arg1 arg2)

    where
      convOp1 :: Op1 -> S.Arg -> S.Oper
      convOp1 = \case
        Asl -> S.Asl

      convOp2 :: Op2 -> S.Arg -> S.Arg -> S.Oper
      convOp2 = \case
        Sub -> S.Sub
        Add -> commute S.Add
        Xor -> commute S.Xor

      commute op a b = if a < b then op a b else op b a
