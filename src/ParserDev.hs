module ParserDev (main) where

import Asm (AsmState(..),Asm,runAsm)
import Control.Monad (when)
import Cost (Cost,costOfCode)
import Data.List (sortBy)
import Data.Map (Map)
import Data.Word (Word8)
import Emulate (MachineState(..),emulate)
import Instruction (Code)
import Par4 (parse)
import Program (Prog(..),Def(..),Exp(..),Id,gram6,exec,Value(..))
import Text.Printf (printf)
import Util (look,extend,zipCheck)
import qualified Asm
import qualified Cost
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Semantics as Sem
import Codegen --(Need,needNothing,needName,needUnion,codegenNew,codegenPredNew,assign,codegenBranch)

type Byte = Word8

main :: FilePath -> IO ()
main file = do
  s <- readFile file
  let prog = parse gram6 s
  go prog

-- select codegenerator (old/new)
old :: Bool
old = False

codegen :: Need -> Sem.Oper -> Asm Sem.Arg
codegen need =
  if old then codegen1 else
    codegenNew need

codegenPred :: Need -> Sem.Pred -> Asm Sem.Arg1
codegenPred need =
  if old then codegenPred1 else
    codegenPredNew need


union :: Need -> Need -> Need
union = needUnion

----------------------------------------------------------------------
-- go

go :: Prog -> IO ()
go prog = do
  print prog

  -- evaluate/emulate on a specific argument value
  let argByte = 7
  let args = [VNum argByte]
  let eres = exec prog "main" args
  printf "evaluation -> %s\n" (show eres)

  --compile...
  let argReg = Sem.RegA
  let targetReg = Sem.RegX -- RegA; ZP 99 -- TODO: perhaps test various targets

  printf "generateCode...\n"
  xs <- generateCodeAlts argReg targetReg prog
  printf "run asm -> #%d alts\n" (length xs)

  -- determine cost of eqch sequence
  let all = orderByCost xs

  _best <- selectCodeAlt all
  let
    tryCode (cost,code) = do
      printf "%s: %s\n" (show cost) (show code)
      let regs = Map.fromList [ (argReg, argByte) ]
      let ms0 = MS { regs, flags = Map.empty }
      let mres = emulate ms0 code targetReg
      printf "emulation -> %s\n" (show mres)
      let same = (VNum mres == eres)
      when (not same) $ printf "*DIFF*\n"
      pure ()

  mapM_ tryCode [ head _best ]


selectCodeAlt :: [(Cost,Code)] -> IO [(Cost,Code)]
selectCodeAlt ys = do
  case ys of
    [] -> error "no alts"
    (lowestCost,_):_ -> do
      let best = takeWhile (\(cost,_) -> cost == lowestCost) ys
      printf "smallest cost = %s, from #%d alternatives\n" (show lowestCost) (length best)
      pure best


orderByCost :: [Code] -> [(Cost,Code)]
orderByCost xs = do
  sortByCost [ (costOfCode code, code) | code <- xs ]
  where
    sortByCost =
      sortBy (\(c1,code1) (c2,code2) ->
                 case Cost.lessTime c1 c2 of
                   EQ -> compare code1 code2 -- order determinism of tests
                   x -> x)

generateCodeAlts :: Sem.Reg -> Sem.Reg -> Prog -> IO [Code]
generateCodeAlts argReg targetReg = \case
  Prog defs -> do
    let regs = [argReg]
    let (names,ss) = Sem.initSS regs -- TODO: this is weird
    --printf "ss=%s\n" (show ss)
    let argName = case names of [x] -> x; _ -> error "argName, not 1"
    let env = collectDefs initialEnv defs
    let main = look "compileProg" env "main"
    let need0 = needNothing
    let
      asm = do
        when old $ (perhaps spillA)
        v <- apply need0 main [ValName8 argName]
        arg <- getArg v
        assign targetReg arg
        -- TODO: rts

    let temps = [Sem.ZeroPage n | n <- [7..19]]
    let state :: AsmState = AsmState { ss, temps }
    printf "calling runAsm...\n"
    let alts = runAsm state asm
    alts



----------------------------------------------------------------------
-- compile

type Env = Map Id Val


collectDefs :: Env -> [Def] -> Env
collectDefs env = \case
  [] -> env
  def@Def{name}:defs -> do
    let dval = ValMacro $ Macro { def, env }
    collectDefs (extend env name dval) defs

compileExp :: Need -> Env -> Exp -> Asm Val
compileExp need env exp = do
  --Asm.Io (printf "compileExp(%s): %s\n" (show need) (show exp))
  v <- compileExp' need env exp
  --Asm.Io (printf "compileExp(%s): %s --> %s\n" (show need) (show exp) (show v))
  pure v

compileExp' :: Need -> Env -> Exp -> Asm Val
compileExp' need env exp = do
 case exp of
  Var x -> pure (look "compileExp/Var" env x)
  Num n -> pure (ValNum n)
  Str s -> undefined s
  Unit -> undefined
  App func [arg1] -> do
    let f = look "compileExp/App1" env func
    act1 <- compileExp need env arg1
    apply need f [act1]
  App func [arg1,arg2] -> do
    let f = look "compileExp/App2" env func
    act1 <- compileExp (need `union` needExp env arg2) env arg1
    act2 <- compileExp (need `union` needVal act1) env arg2
    apply (need) f [act1,act2]
  App _ xs -> error (show ("compileExp/App",length xs))
  Ite i t e -> do
    i <- compileExp (need `union` needExp env t `union` needExp env e) env i
    ite i (compileExp need env t) (compileExp need env e)
  Let x rhs body -> do
    v <- compileExp (need `union` needExp env body) env rhs
    compileExp need (extend env x v) body


needExp :: Env -> Exp -> Need
needExp env = \case
  Var x -> needVal (look "neededBy" env x) -- think this may fail because of how Let is treated below
  Num{} -> needNothing
  Str{} -> needNothing
  Unit -> needNothing
  App _ args -> foldl union needNothing (map (needExp env) args)
  Ite i t e -> undefined i t e
  Let _x rhs body -> needExp env rhs `union` needExp env body


ite :: Val -> Asm Val -> Asm Val -> Asm Val
ite i t e = do
  i <- getArg1 i
  _p1 <- codegenBranch i
  let _p2 = Sem.FlagZ
  Asm.Branch _p1 t e

----------------------------------------------------------------------
-- (Compile time) values

data Prim = Prim (Need -> [Val] -> Asm Val)

instance Show Prim where show Prim{} = "<PRIM>"

data Macro = Macro { def :: Def, env :: Env }
  deriving Show

data Val
  = ValMacro Macro
  | ValPrim Prim
  | ValNum Byte
  | ValName8 Sem.Name
  | ValName1 Sem.Name
  deriving Show

needVal :: Val -> Need
needVal = \case
  ValMacro{} -> needNothing
  ValPrim{} -> needNothing
  ValNum{} -> needNothing
  ValName8 name -> needName name
  ValName1{} -> error "needVal, name1 -- TODO"

valOfArg :: Sem.Arg -> Val
valOfArg = \case
  Sem.Name name -> ValName8 name
  Sem.Imm (Sem.Immediate i) -> ValNum i

valOfArg1 :: Sem.Arg1 -> Val
valOfArg1 = \case
  Sem.Name1 name -> ValName1 name
  -- TODO: we expect to have Sem.Imm1 here

apply :: Need -> Val -> [Val] -> Asm Val
apply need f args =
  case f of
    ValMacro m -> applyMacro need m args
    ValPrim (Prim f) -> f need args
    ValNum{} -> error "apply, number"
    ValName8{} -> error "apply, name8"
    ValName1{} -> error "apply, name1"

applyMacro :: Need -> Macro -> [Val] -> Asm Val
applyMacro need Macro{env,def} actuals = do
  let Def{formals,body} = def
  let binds = zipCheck "applyMacro" formals actuals
  let env' = List.foldl (uncurry . extend) env binds
  compileExp need env' body


initialEnv :: Env
initialEnv = Map.fromList
  [ ("&", ValPrim (binary primAnd))
  , ("+", ValPrim (binary primAdd))
  , ("==", ValPrim (binary primEq))
  , ("shr", ValPrim (unary primShr))
  , ("shl", ValPrim (unary primShl))
  ]

primShl :: Need -> Val -> Asm Val
primShl need v1 = do
  arg1 <- getArg v1
  let oper = Sem.Asl arg1
  res <- codegen need oper
  pure (valOfArg res)

primShr :: Need -> Val -> Asm Val
primShr need v1 = do
  arg1 <- getArg v1
  let oper = Sem.Lsr arg1
  res <- codegen need oper
  pure (valOfArg res)

primAdd :: Need -> Val -> Val -> Asm Val
primAdd need v1 v2 = do
  arg1 <- getArg v1
  arg2 <- getArg v2
  let oper = commute Sem.Add arg1 arg2
  res <- codegen need oper
  pure (valOfArg res)
  where
    commute op a b = if a < b then op a b else op b a -- TODO: should commute be in the Semantics?

primAnd :: Need -> Val -> Val -> Asm Val
primAnd need v1 v2 = do
  arg1 <- getArg v1
  arg2 <- getArg v2
  let oper = commute Sem.And arg1 arg2
  res <- codegen need oper
  pure (valOfArg res)
  where
    commute op a b = if a < b then op a b else op b a

primEq :: Need -> Val -> Val -> Asm Val
primEq need v1 v2 = do
  arg1 <- getArg v1
  arg2 <- getArg v2
  let pred = commute Sem.Equal arg1 arg2
  res <- codegenPred need pred
  pure (valOfArg1 res)
  where
    commute op a b = if a < b then op a b else op b a

unary :: (Need -> Val -> Asm Val) -> Prim
unary op = Prim $ \need -> \case [a] -> op need a; _ -> error "unary"

binary :: (Need -> Val -> Val -> Asm Val) -> Prim
binary op = Prim $ \need -> \case [a,b] -> op need a b; _ -> error "binary"

getArg :: Val -> Asm Sem.Arg -- TODO: needs to be in Asm?
getArg = \case
  ValName8 name -> pure (Sem.Name name)
  ValNum n -> pure (Sem.Imm (Sem.Immediate n))
  ValName1{} -> error "getArg,Name1"
  ValMacro{} -> error "getArg,Macro"
  ValPrim{} -> error "getArg,Prim"

getArg1 :: Val -> Asm Sem.Arg1
getArg1 = \case
  ValName1 name -> pure (Sem.Name1 name)
  ValNum{} -> error "getArg1,Num"
  ValName8{} -> error "getArg1,Name8"
  ValMacro{} -> error "getArg1,Macro"
  ValPrim{} -> error "getArg1,Prim"
