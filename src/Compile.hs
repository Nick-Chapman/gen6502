module Compile
  ( compileTarget,Env
  ) where

import Asm (Asm(..))
import Codegen (preamble,codegen,assign,Reg,Name,Arg(..))
import Data.Map (Map)
import Language (Exp(..),Form(..),Var)
import Text.Printf (printf)
import Util (look)

type Env = Map Var Name

compileTarget :: Env -> Exp -> Reg -> Asm ()
compileTarget env exp reg = do
  preamble
  arg <- compile env exp
  assign reg arg

compile :: Env -> Exp -> Asm Arg
compile env exp = do
  let _ = Print (printf "compile: %s" (show exp))
  case exp of
    Var x -> pure (Name (look "compile" env x))
    Form form -> case form of

      Num n -> codegen (Num n)

      Op1 op1 exp1 -> do
        arg1 <- compile env exp1
        codegen (Op1 op1 arg1)

      Op2 op2 exp1 exp2 -> do
        arg1 <- compile env exp1
        arg2 <- compile env exp2
        codegen (Op2 op2 arg1 arg2)
