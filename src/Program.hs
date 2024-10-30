-- TODO: split parser, pretty-printer, and evaluator from Ast?
module Program
  ( Prog(..), Def(..), Exp(..), Id
  , gram6
  , exec, Value(..)
  ) where

import Data.Bits (xor,(.&.),shiftL,shiftR)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Word (Word8)
import Par4 (Par,noError,alts,many,some,sat)
import Text.Printf (printf)
import Util (look,extend,zipCheck)
import qualified Data.Char as Char (isAlpha,isNumber)
import qualified Data.List as List
import qualified Data.Map as Map

----------------------------------------------------------------------
-- ast

type Byte = Word8

data Prog = Prog [Def]

data Def = Def { name :: Id, formals :: [Id], body :: Exp}
  deriving (Eq)

type Id = String

data Exp
  = Var Id
  | Num Byte
  | Str String
  | Unit
  | App Id [Exp]
  | Ite Exp Exp Exp
  | Let Id Exp Exp
  deriving (Eq)

----------------------------------------------------------------------
-- eval

data Value
  = VNum Byte
  | VBool Bool
  | VClosure Closure
  | VPrim String
  deriving (Eq,Show)

data Closure = Closure { def :: Def, env :: Env }
  deriving (Eq,Show)

type Env = Map Id Value

exec :: Prog -> Id -> [Value] -> Value
exec (Prog defs) main args = do
  let env = collectDefs initialEnv defs
  let f = look "exec" env main
  apply f args

collectDefs :: Env -> [Def] -> Env
collectDefs env = \case
  [] -> env
  def@Def{name}:defs -> do
    let dval = VClosure $ Closure { def, env }
    collectDefs (extend env name dval) defs

eval :: Env -> Exp -> Value
eval env = \case
  Var x -> look "eval/Var" env x
  Num n -> VNum n
  Str s -> undefined s
  Unit -> undefined
  App func args -> do
    let f = look "eval/App" env func
    let actuals = [ eval env arg | arg <- args ]
    apply f actuals
  Ite i t e -> do
    ite (eval env i) (eval env t) (eval env e)
  Let x rhs body -> do
    eval (extend env x (eval env rhs)) body

ite :: Value -> Value -> Value -> Value
ite = \case
  VBool b -> \t e -> if b then t else e
  _ -> error "ite, expect bool"

apply :: Value -> [Value] -> Value
apply f args =
  case f of
    VClosure m -> applyClosure m args
    VPrim fname -> applyPrim (fname,args)
    _ -> error "apply, no closure or prim"

initialEnv :: Env
initialEnv = Map.fromList
  [ (x,VPrim x) | x <- ["&","+","-","^","==","shr","shl"] ]

applyPrim :: (String, [Value]) -> Value
applyPrim = \case
  ("+",[VNum a,VNum b]) -> VNum (a+b)
  ("-",[VNum a,VNum b]) -> VNum (a-b)
  ("&",[VNum a,VNum b]) -> VNum (a .&. b)
  ("^",[VNum a,VNum b]) -> VNum (a `xor` b)
  ("==",[VNum a,VNum b]) -> VBool (a == b)
  ("shl",[VNum a]) -> VNum (a `shiftL` 1)
  ("shr",[VNum a]) -> VNum (a `shiftR` 1)
  x -> error (show ("applyPrim",x))

applyClosure :: Closure -> [Value] -> Value
applyClosure Closure{env,def} actuals = do
  let Def{formals,body} = def
  let binds = zipCheck "applyClosure" formals actuals
  let env' = List.foldl (uncurry . extend) env binds
  eval env' body

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

instance Show Exp where show = intercalate "\n" . pretty


pretty :: Exp -> Lines
pretty = \case
  Var x -> [x]
  Num n -> [show n]
  Str s -> [show s]
  Unit -> ["()"]
  App func [arg1,arg2] | func `elem` infixes ->
    bracket (jux (jux (pretty arg1) [func]) (pretty arg2))
      where infixes = ["+","-","^","=="]
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
  isIdentChar c = Char.isNumber c || isIdentChar1 c || c == '\''

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
