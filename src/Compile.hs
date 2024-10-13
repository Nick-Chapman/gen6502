module Compile
  ( compile0
  ) where

import Asm (Asm(..))
import Codegen (Arg(..),perhaps,spillA,spillX,spillY,locations,codegen)
import Language (Exp(..),Form(..))
import qualified Data.Map as Map

compile0 :: Exp -> Asm Arg
compile0 exp = do
  perhaps spillX
  perhaps spillY
  perhaps spillA
  compile exp
  locations exp
  -- TODO: end up moving result to a specific location, probably A,
  -- as a first step to supporting calling conventions.

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
  state <- GetSemState
  -- TODO share/unify with "locations" / "Located" code
  let located = [ () | (_,exps) <- Map.toList state, exp `elem` exps ]
  if not (null located) then pure True else pure False
