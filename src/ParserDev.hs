module ParserDev (main) where

import Par4 (parse,Par,noError,alts,many,some,sat)
import Data.Char as Char
import Text.Printf (printf)

main :: IO ()
main = do
  putStrLn "*parserDev*"
  let file = "examples/first.ml6"
  --let file = "examples/collatz.ml6"
  s <- readFile file
  let prog = parse6 s
  print prog

parse6 :: String -> Prog
parse6 = parse gram6

gram6 :: Par Prog
gram6 = program where

  fail = alts []

  lit x = do _ <- sat (== x); pure ()
  white = alts [lit ' ',lit '\n'] -- tab?

  digit = digitOfChar <$> sat Char.isDigit
    where
      digitOfChar :: Char -> Int
      digitOfChar c = Char.ord c - ord0 where ord0 = Char.ord '0'

  whitespace = do _ <- many white; return ()

  nibble par = do
    x <- par
    whitespace
    pure x

  -- nibbling from here...

  keywords = ["let","if","then","else"]

  isIdentChar c = Char.isAlpha c || c == '_'

  key s =
    if all isIdentChar s && s `notElem` keywords
    then error (printf "Add \"%s\" to keywords list" s)
    else nibble (noError (mapM_ lit s))

  identifier = noError name
    where
      name = do
        s <- some $ sat isIdentChar
        if s `elem` keywords then fail else nibble (pure s)

  number =
    nibble (foldl (\acc d -> 10*acc + d) 0 <$> some digit)

  -- expression forms...

  bracketed thing = do
    key "("
    x <- thing
    key ")"
    pure x

  var = Var <$> identifier
  num = Num <$> number

  atom = alts [var,num,bracketed exp]

  varOrApp = do
    x <- identifier
    let loop acc = alts [ do x <- atom; loop (x:acc), pure (App x (reverse acc)) ]
    alts [ do y <- atom; loop [y], pure (Var x) ]

  atomOrApp = alts [varOrApp,num,bracketed exp]

  infixOp names sub = sub >>= loop where
    loop acc =
      alts [ pure acc
           , do
               name <- alts [ do key x; return x | x <- names ]
               x <- sub
               loop (Op2 name acc x)
           ]

  sum = infixOp ["+"] atomOrApp
  equal = infixOp ["=="] sum
  conj = infixOp ["&"] equal

  infixWeakestPrecendence = conj

  let_ = do
    key "let"
    x <- identifier
    key "then"
    rhs <- exp
    key "else"
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

  argList =
    alts [ some identifier
         , do key "("; key ")"; pure []
         ]

  definition = do
    key "let"
    name <- identifier
    args <- argList
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
  | App Id [Exp]
  | Ite Exp Exp Exp
  | Let Id Exp Exp
  | Op2 String Exp Exp
  deriving Show
