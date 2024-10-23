module ParserDev (main) where

import Par4 (parse,Par,noError,alts,many,some,sat)
import Text.Printf (printf)
import qualified Data.Char as Char (isAlpha)

main :: FilePath -> IO ()
main file = do
  putStrLn "*parserDev*"
  s <- readFile file
  let prog = parse6 s
  print prog

parse6 :: String -> Prog
parse6 = parse gram6

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
    args <- some pat
    key "="
    body <- exp
    return Def { name, args, body }

  program = do
    whitespace
    defs <- many definition
    pure $ Prog defs


data Def = Def { name :: String, args :: [String], body :: Exp}
  deriving Show

data Prog = Prog [Def]
  deriving Show

type Id = String
data Exp
  = Var Id
  | Num Int
  | Str String
  | Unit
  | App Id [Exp]
  | Ite Exp Exp Exp
  | Let Id Exp Exp
  deriving Show
