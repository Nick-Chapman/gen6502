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
import qualified Codegen as CG

type Byte = Word8

main :: FilePath -> IO ()
main file = do
  s <- readFile file
  let prog = parse gram6 s
  go prog

----------------------------------------------------------------------
-- go

go :: Prog -> IO ()
go prog = do
  print prog

  --compile...
  let argReg = Sem.RegA
  let targetReg = Sem.RegX -- RegA; ZP 99 -- TODO: perhaps test various targets

  printf "generateCode...\n"
  let alts = generateCodeAlts argReg targetReg prog
  best <- selectCodeAlt alts

  -- evaluate/emulate on a specific argument value
  let argByte = 7
  let args = [VNum argByte]
  let eres = exec prog "main" args
  printf "evaluation -> %s\n" (show eres)
  let
    tryCode code = do
      print code
      let regs = Map.fromList [ (argReg, argByte) ]
      let ms0 = MS { regs, flags = Map.empty }
      let mres = emulate ms0 code targetReg
      printf "emulation -> %s\n" (show mres)
      let same = (VNum mres == eres)
      when (not same) $ printf "*DIFF*\n"
      pure ()

  --sequence_ [ tryCode code | code <- best ]
  tryCode (head best)



selectCodeAlt :: [Code] -> IO [Code]
selectCodeAlt xs = do
  printf "run asm -> #%d alts\n" (length xs)
  let ys = orderByCost xs
  case ys of
    [] -> error "no alts"
    (lowestCost,_):_ -> do
      let best = takeWhile (\(cost,_) -> cost == lowestCost) ys
      printf "smallest cost = %s, from #%d alternatives\n" (show lowestCost) (length best)
      pure [ code | (_,code) <- best ]


orderByCost :: [Code] -> [(Cost,Code)]
orderByCost xs = do
  sortByCost [ (costOfCode code, code) | code <- xs ]
  where
    sortByCost =
      sortBy (\(c1,code1) (c2,code2) ->
                 case Cost.lessTime c1 c2 of
                   EQ -> compare code1 code2 -- order determinism of tests
                   x -> x)

generateCodeAlts :: Sem.Reg -> Sem.Reg -> Prog -> [Code]
generateCodeAlts argReg targetReg = \case
  Prog defs -> do
    let regs = [argReg]
    let (names,ss) = Sem.initSS regs -- TODO: this is weird
    let argName = case names of [x] -> x; _ -> error "argName, not 1"
    let env = collectDefs initialEnv defs
    let main = look "compileProg" env "main"
    let
      asm = do
        preamble
        v <- apply main [ValName8 argName]
        arg <- getArg v
        CG.assign targetReg arg
        -- TODO: rts

    let temps = [Sem.ZeroPage n | n <- [7..19]]
    let state :: AsmState = AsmState { ss, temps }
    runAsm state asm


preamble :: Asm ()
preamble = do
  perhaps CG.spillA
  --perhaps CG.spillX
  --perhaps CG.spillY
  pure ()

perhaps :: Asm () -> Asm ()
perhaps a = Asm.Alt (pure ()) a

----------------------------------------------------------------------
-- compile

type Env = Map Id Val


collectDefs :: Env -> [Def] -> Env
collectDefs env = \case
  [] -> env
  def@Def{name}:defs -> do
    let dval = ValMacro $ Macro { def, env }
    collectDefs (extend env name dval) defs

applyMacro :: Macro -> [Val] -> Asm Val
applyMacro Macro{env,def} actuals = do
  let Def{formals,body} = def
  let binds = zipCheck "applyMacro" formals actuals
  let env' = List.foldl (uncurry . extend) env binds
  compileExp env' body

compileExp :: Env -> Exp -> Asm Val
compileExp env = \case
  Var x -> pure (look "compileExp/Var" env x)
  Num n -> pure (ValNum n)
  Str s -> undefined s
  Unit -> undefined
  App func args -> do
    let f = look "compileExp/App" env func
    actuals <- sequence [ compileExp env arg | arg <- args ]
    apply f actuals
  Ite i t e -> do
    i <- compileExp env i
    ite i (compileExp env t) (compileExp env e)
  Let x rhs body -> do
    v <- compileExp env rhs
    compileExp (extend env x v) body

ite :: Val -> Asm Val -> Asm Val -> Asm Val
ite i t e = do
  i <- getArg1 i
  _p1 <- CG.codegenBranch i
  let _p2 = Sem.FlagZ
  Asm.Branch _p1 t e

----------------------------------------------------------------------
-- (Compile time) values

data Prim = Prim ([Val] -> Asm Val)
data Macro = Macro { def :: Def, env :: Env }

data Val
  = ValMacro Macro
  | ValPrim Prim
  | ValNum Byte
  | ValName8 Sem.Name
  | ValName1 Sem.Name

valOfArg :: Sem.Arg -> Val
valOfArg = \case
  Sem.Name name -> ValName8 name
  Sem.Imm (Sem.Immediate i) -> ValNum i

valOfArg1 :: Sem.Arg1 -> Val
valOfArg1 = \case
  Sem.Name1 name -> ValName1 name
  -- TODO: we expect to have Sem.Imm1 here

apply :: Val -> [Val] -> Asm Val
apply f args =
  case f of
    ValMacro m -> applyMacro m args
    ValPrim (Prim f) -> f args
    ValNum{} -> error "apply, number"
    ValName8{} -> error "apply, name8"
    ValName1{} -> error "apply, name1"

initialEnv :: Env
initialEnv = Map.fromList
  [ ("&", ValPrim (binary primAnd))
  , ("+", ValPrim (binary primAdd))
  , ("==", ValPrim (binary primEq))
  , ("shr", ValPrim (unary primShr))
  , ("shl", ValPrim (unary primShl))
  ]

primShl :: Val -> Asm Val
primShl v1 = do
  arg1 <- getArg v1
  let oper = Sem.Asl arg1
  res <- CG.codegen oper
  pure (valOfArg res)

primShr :: Val -> Asm Val
primShr v1 = do
  arg1 <- getArg v1
  let oper = Sem.Lsr arg1
  res <- CG.codegen oper
  pure (valOfArg res)

primAdd :: Val -> Val -> Asm Val
primAdd v1 v2 = do
  arg1 <- getArg v1
  arg2 <- getArg v2
  let oper = commute Sem.Add arg1 arg2
  res <- CG.codegen oper
  pure (valOfArg res)
  where
    commute op a b = if a < b then op a b else op b a -- TODO: should commute be in the Semantics?

primAnd :: Val -> Val -> Asm Val
primAnd v1 v2 = do
  arg1 <- getArg v1
  arg2 <- getArg v2
  let oper = commute Sem.And arg1 arg2
  res <- CG.codegen oper
  pure (valOfArg res)
  where
    commute op a b = if a < b then op a b else op b a

primEq :: Val -> Val -> Asm Val
primEq v1 v2 = do
  arg1 <- getArg v1
  arg2 <- getArg v2
  let pred = commute Sem.Equal arg1 arg2
  res <- CG.codegenPred pred
  pure (valOfArg1 res)
  where
    commute op a b = if a < b then op a b else op b a

unary :: (Val -> Asm Val) -> Prim
unary op = Prim $ \case [a] -> op a; _ -> error "unary"

binary :: (Val -> Val -> Asm Val) -> Prim
binary op = Prim $ \case [a,b] -> op a b; _ -> error "binary"

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
