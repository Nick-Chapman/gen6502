
-- TODO: kill this now unused v1 compile.
module Compile
  ( compileTarget,Env
  ) where

import Asm (Asm(..))
import Codegen (preamble,codegen1,codegenPred1,codegenBranch,assign,Reg,Arg)
import Data.Map (Map)
import Language (Exp(..),Form(..),Op2(..),Op1(..),Var,Pred(..))
import Semantics (Arg1)
import Util (look,extend)
import qualified Semantics as Sem

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
    codegenPred1 (Sem.Equal arg1 arg2)

compile :: Env -> Exp -> Asm Arg
compile env exp = do
  case exp of
    Var x -> pure (look "compile" env x)

    Let x rhs body -> do
      rhs <- compile env rhs
      compile (extend env x rhs) body

    If p exp1 exp2 -> do
      p <- compileP env p
      _p <- codegenBranch p
      --Branch _p (compile env exp1) (compile env exp2)
      Branch Sem.FlagZ (compile env exp1) (compile env exp2)

    Form form -> case form of

      Num n -> codegen1 (Sem.ONum n)

      Op1 op1 exp1 -> do
        arg1 <- compile env exp1
        codegen1 (convOp1 op1 arg1)

      Op2 op2 exp1 exp2 -> do
        arg1 <- compile env exp1
        arg2 <- compile env exp2
        codegen1 (convOp2 op2 arg1 arg2)

    where
      convOp1 :: Op1 -> Sem.Arg -> Sem.Oper
      convOp1 = \case
        Asl -> Sem.Asl

      convOp2 :: Op2 -> Sem.Arg -> Sem.Arg -> Sem.Oper
      convOp2 = \case
        Sub -> Sem.Sub
        Add -> commute Sem.Add
        Xor -> commute Sem.Eor

      commute op a b = if a < b then op a b else op b a
