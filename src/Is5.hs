module Is5 (main) where

import Asm (Cost,Asm(..),runAsm,Temps(..))
import Emulate (Env,MachineState,initMS,emulate)
import Instruction (Code,Instruction(..),ITransfer(..),ICompute(..),Arg(..),Loc(..),ZeroPage(..),Immediate(..),SemState,noSemantics,transferSemantics,computeSemantics)
import Language (Exp(..),Form(..),Op1(..),Op2(..),Var,EvalEnv,eval)
import Util (extend)

import qualified Data.Map as Map
import Text.Printf (printf)

-- TODO: split out modules for: Codegen, Compilation, and testing

main :: IO ()
main = do
  print "*Is4*"
  runTests

runTests :: IO ()
runTests = do

  let a = "a"
  let x = "x"
  let y = "y"
  let z = "z"
  let z2 = "z2"

  let env :: Env = Map.fromList [(a,RegA), (x,RegX), (y,RegY), (z,ZP 1), (z2,ZP 2)]
  let ee :: EvalEnv = Map.fromList [(a,13),(x,42),(y,101),(z,19),(z2,28)]

  let num n = Exp (Num n)
  let var x = Exp (Var x)
  let add e1 e2 = Exp (Op2 Add e1 e2)
  let asl e = Exp (Op1 Asl e)
  let let_ x e1 e2 = Exp (Let x e1 e2)

  let
    examples =
      [ num 77

      , var a
      , var x
      , var y
      , var z

      , add (var a) (num 1)
      , add (var x) (num 1)
      , add (var y) (num 1)
      , add (var z) (num 1)

      , add (num 1) (var a)
      , add (num 1) (var x)
      , add (num 1) (var y)
      , add (num 1) (var z)

      , add (var a) (num 2)
      , add (var x) (num 2)
      , add (var y) (num 2)
      , add (var z) (num 2)

      , add (num 3) (var a)
      , add (num 3) (var x)
      , add (num 3) (var y)
      , add (num 3) (var z)

      , add (var a) (var a)
      , add (var a) (var x)
      , add (var a) (var y)
      , add (var a) (var z)

      , add (var x) (var a)
      , add (var x) (var x)
      , add (var x) (var y)
      , add (var x) (var z)

      , add (var y) (var a)
      , add (var y) (var x)
      , add (var y) (var y)
      , add (var y) (var z)

      , add (var z) (var a)
      , add (var z) (var x)
      , add (var z) (var y)
      , add (var z) (var z)

      , add (var z) (var z2)
      , add (num 17) (num 19)
      , add (num 14) (num 1)
      , add (num 1) (num 14)

      , asl (num 14)
      , asl (var a)
      , asl (var x)
      , asl (var y)
      , asl (var z)

      , add (asl (var a)) (var a)
      , add (asl (var x)) (var x)
      , add (asl (var y)) (var y)
      , add (asl (var z)) (var z)

      , add (add (var a) (num 1)) (add (var a) (num 1))
      , add (add (num 17) (num 19)) (add (num 17) (num 19))

      , let_ "t1" (var a) (var "t1")
      , let_ "t1" (var x) (var "t1")
      , let_ "t1" (var y) (var "t1")
      , let_ "t1" (var z) (var "t1")

      , let_ "t1" (var a) (add (var "t1") (var "t1"))
      , let_ "t1" (var x) (add (var "t1") (var "t1"))
      , let_ "t1" (var y) (add (var "t1") (var "t1"))
      , let_ "t1" (var z) (add (var "t1") (var "t1"))

      , let_ "t1" (asl (add (num 1) (var z))) (add (var z2) (var "t1"))

      , add (num 1) (var z)

      ]

  printf "(eval)env = %s\n" (show ee)
  let ms0 :: MachineState = initMS env ee
  printf "ms0 = %s\n" (show ms0)

  let ss = semStateOfEnv env
  mapM_ (run1 ss ee ms0) examples

semStateOfEnv :: Env -> SemState
semStateOfEnv env = Map.fromList [ (loc,[Exp (Var x)]) | (x,loc) <- Map.toList env ]

run1 :: SemState -> EvalEnv -> MachineState -> Exp -> IO ()
run1 state ee ms0 example = do
  printf "\nexample = %s\n" (show example)
  let eres = eval ee example
  --printf "eres = %d\n" eres
  let temps1 = Temps [ZeroPage n | n <- [7..19]]
  let rs :: [Res] = runAsm temps1 state (compile0 example)
  let num = length rs
  if num == 0 then error "#results==0" else pure ()
  let
    prRes :: (Int, Res) -> IO ()
    prRes (i,(code,cost,arg)) = do
      let mres = emulate ms0 (code,arg)
      let same = (mres == eres)
      let ok :: String = if same then " {ok}" else printf " {FAIL: different: %d}" mres
      printf "#%d:{%s}: %s --> %s%s\n" i (show cost) (show code) (show arg) ok
      if not same then error "not ok" else pure ()

  mapM_ prRes (zip [1..] rs)

type Res = (Code,Cost,Arg)

compile0 :: Exp -> Asm Arg
compile0 exp = do
  perhaps spillA
  perhaps spillX
  perhaps spillY
  compile exp

compile :: Exp -> Asm Arg
compile exp@(Exp form) = do
  reusePreviousCompilation exp >>= \case
    Just arg -> pure arg
    Nothing ->
      case form of
        Num n -> pure (Imm (Immediate n))
        Var{} -> locations exp

        Op1 op1 exp1-> do
          arg <- compile exp1
          codegen exp (Op1 op1 arg)
          locations exp

        Op2 op2 exp1 exp2 -> do
          _ <- compile exp1
          arg2 <- compile exp2
          arg1 <- locations exp1
          codegen exp (Op2 op2 arg1 arg2)
          locations exp

        Let x rhs body -> do
          arg <- compile rhs
          link x arg
          --checkFreeVarsLocatable body -- shortcut the fail. nesc?
          compile body
{-
free :: Exp -> [Var]
free = undefined

checkFreeVarsLocatable :: Exp -> Asm ()
checkFreeVarsLocatable exp = do
  state <- GetSemState
  let xs = free exp
  if all id [ somewhere (locateV state x) | x <- xs ] then pure () else Nope

locateV :: SemState -> Var -> Located
locateV s x = locateE s (Exp (Var x))
-}

link :: Var -> Arg -> Asm ()
link x = \case
  Imm{} -> pure()
  Loc loc -> updateSemState (linkName x loc)

-- TODO : Have Asm primitive to link Var(Exp) to a Location. Avoid need for {Get,Set}SemanticState
linkName :: Var -> Loc -> SemState -> SemState
linkName x loc s = do
  let e = Exp (Var x)
  let es = case Map.lookup loc s of Just es -> es; Nothing -> []
  extend s loc (e:es)

updateSemState :: (SemState -> SemState) -> Asm ()
updateSemState f = do
  ss <- GetSemState
  SetSemState (f ss)

locations :: Exp -> Asm Arg
locations exp = do
  state <- GetSemState
  let located = locateE state exp
  locationsL located

reusePreviousCompilation :: Exp -> Asm (Maybe Arg)
reusePreviousCompilation exp = do
  state <- GetSemState
  let located = locateE state exp
  if somewhere located then Just <$> (locationsL located) else pure Nothing

somewhere :: Located -> Bool
somewhere located = not (null (argsL located))

locationsL :: Located -> Asm Arg
locationsL located = alts [ pure arg | arg <- argsL located ]

data Located = LocatedList [Arg]

argsL :: Located -> [Arg]
argsL (LocatedList xs) = xs

locateE :: SemState -> Exp -> Located
locateE state exp =
  LocatedList
  ((case exp of Exp (Num b) -> [Imm (Immediate b)]; _ -> []) -- TODO: surprise we need this
   ++ [ Loc loc | (loc,exps) <- Map.toList state, exp `elem` exps ])

alts :: [Asm a] -> Asm a
alts = foldl Alt Nope

----------------------------------------------------------------------
-- instruction selection

type Gen = Exp -> Form Arg -> Asm ()

select :: [Gen] -> Gen
select gs = \e f -> alts [ g e f | g <- gs ]

codegen :: Gen
codegen = select [doubling,addition,incrementX,incrementM]

doubling :: Gen
doubling e = \case
  Op1 Asl arg -> do loadA arg; comp e Asla
  _ -> Nope

addition :: Gen
addition e = \case
  Op2 Add (Loc RegA) arg -> do addIntoA e arg
  Op2 Add arg (Loc RegA) -> do addIntoA e arg
  Op2 Add arg1 arg2 -> do loadA arg1; addIntoA e arg2 --; perhaps spillA -- TODO
  _ -> Nope

addIntoA :: Exp -> Arg -> Asm ()
addIntoA e = \case
  Imm imm -> do clc; comp e (Adci imm)
  Loc RegA -> comp e (Asla)
  Loc RegX -> Nope
  Loc RegY -> Nope
  Loc (ZP z) -> do clc; comp e  (Adcz z)

incrementX :: Gen -- TODO Y like X
incrementX e = \case
  Op2 Add arg (Imm 1) -> do loadX arg; comp e Inx
  Op2 Add (Imm 1) arg -> do loadX arg; comp e Inx
  _ -> Nope

incrementM :: Gen
incrementM e = \case
  Op2 Add arg (Imm 1) -> do z <- inZP arg; comp e (Incz z)
  Op2 Add (Imm 1) arg -> do z <- inZP arg; comp e (Incz z)
  _ -> Nope

inZP :: Arg -> Asm ZeroPage
inZP = \case
  Loc (ZP z) -> pure z
  _ -> Nope

-- TODO: compile time constant folding

----------------------------------------------------------------------
-- codegen helpers

loadA :: Arg -> Asm ()
loadA = \case
  Imm imm -> trans (Ldai imm)
  Loc RegA -> pure ()
  Loc RegX -> trans Txa
  Loc RegY -> trans Tya
  Loc (ZP z) -> trans (Ldaz z)

loadX :: Arg -> Asm ()
loadX = \case
  Imm imm -> trans (Ldxi imm)
  Loc RegA -> trans Tax
  Loc RegX -> pure ()
  Loc RegY -> Nope -- no Y->X
  Loc (ZP z) -> trans (Ldxz z)

clc :: Asm ()
clc = Emit Clc noSemantics

trans :: ITransfer -> Asm ()
trans i = Emit (Tx i) (transferSemantics i)

comp :: Exp -> ICompute -> Asm ()
comp e i = Emit (Comp i) (computeSemantics e i)

----------------------------------------------------------------------
-- spilling...

perhaps :: Asm () -> Asm ()
perhaps a = alts [a, pure ()] -- shorter come last; nicer for early dev/debug

-- TODO: only spill if these location map to some expression

spillA :: Asm ()
spillA = do
  z <- Fresh
  trans (Sta z)

spillX :: Asm ()
spillX = do
  z <- Fresh
  trans (Stx z)

spillY :: Asm ()
spillY = do
  z <- Fresh
  trans (Sty z)
