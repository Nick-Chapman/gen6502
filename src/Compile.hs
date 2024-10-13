module Compile
  ( compile0
  ) where

import Asm (Asm(..))
import Codegen (Arg(..),everywhere,perhaps,spillA,spillX,spillY,locations,codegen,alts)
import Instruction (Loc(..),SemState)
import Language (Exp(..),Form(..))
import Util (extend)
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

havePreviousCompilation :: Exp -> Asm Bool
havePreviousCompilation exp = do
  state <- GetSemState
  -- TODO share/unify with "locations" / "Located" code
  let located = [ () | (_,exps) <- Map.toList state, exp `elem` exps ]
  if not (null located) then pure True else pure False

compile,compileU :: Exp -> Asm ()
compile exp = do
  havePreviousCompilation exp >>= \case
    True -> pure ()
    False -> compileU exp

-- TODO: pass an Env to deal with user let bindings

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

    -- TODO redo Let exps to pay no attention to user vars,
    -- and just recompile the exp every tume
    -- but finding the previous compilation anyway
    Let x rhs body -> do
      compile rhs
      -- TODO: dont do linkage with non-determinism (alts)
      arg <- locations rhs
      let e = Exp (Var x)
      linkE e arg
      compile body
      loc <- locations body
      linkE exp loc


linkE :: Exp -> Arg -> Asm ()
linkE e = \case
  Imm{} -> pure()
  MLoc located -> do
    loc <- alts [ pure loc | loc <- (everywhere located) ] -- short term hack
    updateSemState (linkExp e loc)

-- TODO : Have Asm primitive to link Var(Exp) to a Location. Avoid need for {Get,Set}SemanticState
linkExp :: Exp -> Loc -> SemState -> SemState
linkExp e loc s = do
  let es = case Map.lookup loc s of Just es -> es; Nothing -> []
  extend s loc (e:es)

updateSemState :: (SemState -> SemState) -> Asm ()
updateSemState f = do
  ss <- GetSemState
  SetSemState (f ss)
