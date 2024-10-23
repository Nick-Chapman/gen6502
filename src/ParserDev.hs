module ParserDev (main) where

import Asm (AsmState(..),Asm,runAsm)
import Codegen (codegen,codegenPred,codegenBranch)
import Cost (Cost,costOfCode,lessTime)
import Data.List (intercalate)
import Data.List (sortBy)
import Data.Map (Map)
import Data.Word (Word8)
import Instruction (Code)
import Par4 (parse,Par,noError,alts,many,some,sat)
import Text.Printf (printf)
import Util (look,extend)
import qualified Data.Char as Char (isAlpha)
import qualified Data.List as List
import qualified Data.Map as Map

import qualified Semantics as Sem
import qualified Asm

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
  code <- generateCode prog
  ppCode code

ppCode :: Code -> IO ()
ppCode code =
  print code

generateCode :: Prog -> IO Code
generateCode prog = do
  printf "generateCode...\n"
  let asm = compileProg prog

  -- calling main: no vars in no regs
  let regs = []
  let (_names,ss) = Sem.initSS regs
  let temps = [Sem.ZeroPage n | n <- [7..19]]
  let state :: AsmState = AsmState { ss, temps }
  --let vars = []
  --let env = Map.fromList [ (x,Sem.Name name) | (x,name) <- zip vars names ]

  let xs = runAsm state asm
  printf "run asm -> #%d alts\n" (length xs)
  let ys = orderByCost xs
  case ys of
    [] -> error "no alts"
    (cost,code):_ -> do
      printf "smallest cost = %s\n" (show cost)
      pure code

orderByCost :: [Code] -> [(Cost,Code)]
orderByCost xs = do
  sortByCost [ (costOfCode code, code) | code <- xs ]
  where
    sortByCost =
      sortBy (\(c1,code1) (c2,code2) ->
                 case lessTime c1 c2 of
                   EQ -> compare code1 code2 -- order determinism of tests
                   x -> x)

----------------------------------------------------------------------
-- Ast

data Prog = Prog [Def]

data Def = Def { name :: Id, formals :: [Id], body :: Exp}

type Id = String

data Exp
  = Var Id
  | Num Byte
  | Str String
  | Unit
  | App Id [Exp]
  | Ite Exp Exp Exp
  | Let Id Exp Exp

----------------------------------------------------------------------
-- compile

type Env = Map Id Val

compileProg :: Prog -> Asm ()
compileProg = \case
  Prog defs -> do
    let env = collectDefs initialEnv defs
    let main = look "compileProg" env "main"
    _val <- apply main [] -- TODO: do what with result? -- rts !
    pure ()

collectDefs :: Env -> [Def] -> Env
collectDefs env = \case
  [] -> env
  def@Def{name}:defs -> do
    let dval = ValMacro $ Macro { def, env }
    collectDefs (extend env name dval) defs

applyMacro :: Macro -> [Val] -> Asm Val
applyMacro Macro{env,def} actuals = do
  let Def{formals,body} = def
  let binds = zip formals actuals -- TODO: check length
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
  Let{} -> undefined

ite :: Val -> Asm Val -> Asm Val -> Asm Val
ite i t e = do
  i <- getArg1 i
  _p1 <- codegenBranch i
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
  res <- codegen oper
  pure (valOfArg res)

primShr :: Val -> Asm Val
primShr v1 = do
  arg1 <- getArg v1
  let oper = Sem.Asl arg1 -- TODO: bug, should be Lsr
  res <- codegen oper
  pure (valOfArg res)

primAdd :: Val -> Val -> Asm Val
primAdd v1 v2 = do
  arg1 <- getArg v1
  arg2 <- getArg v2
  let oper = commute Sem.Add arg1 arg2
  res <- codegen oper
  pure (valOfArg res)
  where
    commute op a b = if a < b then op a b else op b a -- should this be in the Semantics?

primAnd :: Val -> Val -> Asm Val
primAnd v1 v2 = do
  arg1 <- getArg v1
  arg2 <- getArg v2
  let oper = commute Sem.Xor arg1 arg2 -- TODO: bug, should be And
  res <- codegen oper
  pure (valOfArg res)
  where
    commute op a b = if a < b then op a b else op b a -- should this be in the Semantics?

primEq :: Val -> Val -> Asm Val
primEq v1 v2 = do
  arg1 <- getArg v1
  arg2 <- getArg v2
  let pred = commute Sem.Equal arg1 arg2
  res <- codegenPred pred
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

----------------------------------------------------------------------

gram6 :: Par Prog
gram6 = program where

  fail = alts []

  lit x = do _ <- sat (== x); pure ()
  white = alts [lit ' ',lit '\n'] -- TODO: tabs

  decDigit = alts [ do lit c; pure n | (c,n) <- zip "0123456789" [0..] ]
  hexDigit = alts [ do lit c; pure n | (c,n) <- zip "0123456789abcdef" [0..] ]

  whitespace = do _ <- many white; return ()

  nibble par = do
    x <- par
    whitespace
    pure x

  -- nibbling from here...

  keywords = ["let","in","if","then","else"]

  isIdentChar1 c = Char.isAlpha c || c == '_'
  isIdentChar c = isIdentChar1 c || c == '\'' -- TODO digits

  key s =
    if all isIdentChar s && s `notElem` keywords
    then error (printf "Add \"%s\" to keywords list" s)
    else nibble (noError (mapM_ lit s))

  identifier = noError name
    where
      name = do
        x <- sat isIdentChar1
        xs <- many $ sat isIdentChar
        let s = x:xs
        if s `elem` keywords then fail else nibble (pure s)

  decNumber = do
    foldl (\acc d -> 10*acc + d) 0 <$> some decDigit

  hexNumber = noError $ do
    lit '0'
    lit 'x'
    foldl (\acc d -> 16*acc + d) 0 <$> some hexDigit

  number = nibble $ alts [hexNumber,decNumber]

  doubleQuote = '"'
  stringLitChar = sat $ \c -> c /= doubleQuote

  string = nibble $ do
    lit doubleQuote
    x <- many stringLitChar
    lit doubleQuote
    pure x

  openClose = noError $ do
    key "("
    key ")"

  pat = alts [identifier, do openClose; pure "_"]

  -- expression forms...

  bracketed thing = do
    key "("
    x <- thing
    key ")"
    pure x

  var = Var <$> identifier
  num = Num <$> number
  str = Str <$> string
  unit = do openClose; pure Unit

  atom = alts [num,str,unit,bracketed exp]

  atomOrVar = alts [atom,var]

  varOrApp = do
    x <- identifier
    let loop acc = alts [ do x <- atomOrVar; loop (x:acc), pure (App x (reverse acc)) ]
    alts [ do y <- atomOrVar; loop [y], pure (Var x) ]

  atomOrApp = alts [atom,varOrApp]

  infixOp names sub = sub >>= loop where
    loop acc =
      alts [ pure acc
           , do
               name <- alts [ do key x; return x | x <- names ]
               x <- sub
               loop (App name [acc,x])
           ]

  sum = infixOp ["+"] atomOrApp
  equal = infixOp ["=="] sum
  conj = infixOp ["&"] equal

  infixWeakestPrecendence = conj

  let_ = do
    key "let"
    x <- pat
    key "="
    rhs <- exp
    key "in"
    body <- exp
    pure (Let x rhs body)

  ite = do
    key "if"
    i <- exp
    key "then"
    t <- exp
    key "else"
    e <- exp
    pure (Ite i t e)

  exp = alts [ite,let_,infixWeakestPrecendence]

  definition = do
    key "let"
    name <- identifier
    formals <- some pat
    key "="
    body <- exp
    return Def { name, formals, body }

  program = do
    whitespace
    defs <- many definition
    pure $ Prog defs

----------------------------------------------------------------------
-- pretty print

instance Show Prog where
  show (Prog defs) =
    "\n" ++ intercalate "\n" (map show defs)

instance Show Def where
  show Def{name,formals,body} =
    unlines $ indented
    (printf "let %s %s =" name (intercalate " " formals))
    (pretty body)

instance Show Exp where show = unlines . pretty

pretty :: Exp -> Lines
pretty = \case
  Var x -> [x]
  Num n -> [show n]
  Str s -> [show s]
  Unit -> ["()"]
  App func args ->
    bracket (foldl jux [] ([func] : map pretty args))
  Ite i t e ->
    bracket (indented "if" (pretty i) ++ indented "then" (pretty t) ++ indented "else" (pretty e))
  Let x rhs body ->
    indented ("let " ++ x ++ " =") (pretty rhs) ++ pretty body

type Lines = [String]

jux :: Lines -> Lines -> Lines
jux [x] [y] = [ x ++ " " ++ y ]
jux xs ys = xs ++ ys

bracket :: Lines -> Lines
bracket = onHead ("(" ++) . onTail (++ ")")

onHead,onTail :: (String -> String) -> Lines -> Lines
onHead _ [] = error "onHead"
onHead f (x:xs) = f x : xs
onTail f = reverse . onHead f . reverse

indented :: String -> Lines -> Lines
indented hang = \case
  [] -> error "indented"
  [oneLine] -> [hang ++ " " ++ oneLine]
  lines -> [hang] ++ ["  " ++ line | line <- lines]
