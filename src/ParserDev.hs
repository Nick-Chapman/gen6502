module ParserDev (main) where

import Par4 (parse,Par,noError,alts,many,some,sat)
import Text.Printf (printf)
import qualified Data.Char as Char (isAlpha)
import Data.List (intercalate)

import Instruction
import Asm
import Cost
import Data.List (sortBy)
import Util (look,extend)

import Data.Map (Map)
import qualified Data.Map as Map

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
  let state :: AsmState = undefined
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
  | Num Int
  | Str String
  | Unit
  | App Id [Exp]
  | Ite Exp Exp Exp
  | Let Id Exp Exp

----------------------------------------------------------------------
-- compile

data DVal = DefClosure { def :: Def, denv :: Denv }

type Denv = Map Id DVal

compileProg :: Prog -> Asm ()
compileProg = \case
  Prog defs -> do
    let main = "main"
    let denv = collectDefs Map.empty defs
    _val <- compileDefApp denv main [] -- TODO: do what with result?
    pure ()

collectDefs :: Denv -> [Def] -> Denv
collectDefs denv = \case
  [] -> denv
  def@Def{name}:defs -> do
    let dval = DefClosure { def, denv }
    collectDefs (extend denv name dval) defs

compileDefApp :: Denv -> Id -> [Val] -> Asm Val
compileDefApp denv0 name actuals = do
  let DefClosure{denv,def} = look "compileDefApp" denv0 name
  let Def{formals,body} = def
  let env = Map.fromList (zip formals actuals) -- TODO: check length
  compileExp denv env body

data Val = VAL -- TODO

type Env = Map Id Val

compileExp :: Denv -> Env -> Exp -> Asm Val
compileExp denv env = \case
  Var x -> undefined x env
  Num n -> undefined n VAL -- HERE
  Str s -> undefined s
  Unit -> undefined
  App func args -> do
    actuals <- sequence [ compileExp denv env arg | arg <- args ]
    compileDefApp denv func actuals
  Ite{} -> undefined
  Let{} -> undefined


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
