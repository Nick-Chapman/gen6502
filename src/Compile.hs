module Compile
  ( compileTarget
  ) where

import Asm (Asm(..))
import Codegen (preamble,codegen,locations,assign,Reg)
import Language (Exp(..),Form(..))
import Text.Printf (printf)

compileTarget :: Exp -> Reg -> Asm ()
compileTarget exp reg = do
  preamble
  compile exp
  arg <- locations exp
  Codegen.assign reg arg

compile :: Exp -> Asm ()
compile exp = do
  regs <- Holding exp
  case regs of
    [] -> compile1 exp
    _:_ -> pure () -- common subexpression elimination

compile1 :: Exp -> Asm ()
compile1 exp@(Exp form) = do
  let _ = Print (printf "compile: %s" (show exp))
  case form of
    Num{} -> pure ()
    Var{} -> pure ()
    Op1 op1 exp1 -> do
      compile exp1
      arg1 <- locations exp1
      codegen exp (Op1 op1 arg1)
    Op2 op2 exp1 exp2 -> do
      compile exp1
      compile exp2
      arg1 <- locations exp1
      arg2 <- locations exp2
      codegen exp (Op2 op2 arg1 arg2)
