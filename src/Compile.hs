module Compile
  ( compileTarget
  ) where

import Asm (Asm(..))
import Codegen (preamble,codegen,locations,assign,Reg)
import Language (Exp(..),Form(..))
import qualified Data.Map as Map

compileTarget :: Exp -> Reg -> Asm ()
compileTarget exp reg = do
  preamble
  compile exp
  arg <- locations exp
  Codegen.assign reg arg

compile :: Exp -> Asm ()
compile exp = do
  havePreviousCompilation exp >>= \case
    True -> pure ()
    False -> compileU exp

compileU :: Exp -> Asm ()
compileU exp@(Exp form) = do
  case form of
    Num{} -> pure ()
    Var{} -> pure ()
    Op1 op1 exp1 -> do
      compile exp1
      arg <- locations exp1
      codegen exp (Op1 op1 arg)
    Op2 op2 exp1 exp2 -> do
      compile exp1
      compile exp2
      arg1 <- locations exp1
      arg2 <- locations exp2
      codegen exp (Op2 op2 arg1 arg2)

havePreviousCompilation :: Exp -> Asm Bool
havePreviousCompilation exp = do
  -- TODO share/unify with "locations" / "Located" code
  state <- GetSemState -- TODO: should not use this. It is a cogen concept
  let located = [ () | (_,exps) <- Map.toList state, exp `elem` exps ]
  if not (null located) then pure True else pure False
