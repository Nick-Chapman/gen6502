module Is5 (main) where

import Asm (Asm(..),runAsm,Temps(..))
import Control.Monad (when)
import Emulate (Env,MachineState,initMS,emulate)
import Instruction (Instruction(..),ITransfer(..),ICompute(..),Arg(..),Loc(..),ZeroPage(..),Immediate(..),SemState,noSemantics,transferSemantics,computeSemantics)
import Language (Exp(..),Form(..),Op1(..),Op2(..),EvalEnv,eval)
import Text.Printf (printf)
import Util (extend)
import qualified Cost
import qualified Data.Map as Map
import qualified Data.List as List

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
  let xor e1 e2 = Exp (Op2 Xor e1 e2)
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

      , xor (var a) (asl (var a))
      , xor (var a) (asl (asl (var a)))
--      , xor (xor (var a) (asl (var a))) (asl (asl (var a))) -- TODO: spill needed
      ]

  printf "(eval)env = %s\n" (show ee)
  let ms0 :: MachineState = initMS env ee
  printf "ms0 = %s\n" (show ms0)

  let ss = semStateOfEnv env
  mapM_ (run1 ss ee ms0) (last 1000 (zip [1::Int ..] examples))
    where last n xs = reverse (take n (reverse xs))

semStateOfEnv :: Env -> SemState
semStateOfEnv env = Map.fromList [ (loc,[Exp (Var x)]) | (x,loc) <- Map.toList env ]


run1 :: SemState -> EvalEnv -> MachineState -> (Int,Exp) -> IO ()
run1 state ee ms0 (i,example) = do
  printf "\n[%d]example = %s\n" i (show example)
  let eres = eval ee example
  let temps1 = Temps [ZeroPage n | n <- [7..19]]
  rs <- runAsm Cost.lessTime temps1 state (compile0 example)
  let _num = length rs
  if _num == 0 then error "#results==0" else pure ()

  let
    prRes ((code,cost,arg),mres) = do
      let same = (mres == eres)
      let ok :: String = if same then "" else printf " {FAIL: different: %d}" mres
      printf "{%s}: %s --> %s%s\n" (show cost) (show code) (show arg) ok

  let rsWithEmu = [ (r, emulate ms0 (code,arg)) | r@(code,_,arg) <- rs ]
  let (ok,bad) = List.partition correct rsWithEmu
        where correct (_,mres) = (mres==eres)

  when (length bad > 0) $ do
    printf "eval -> %d\n" eres

  let n1 = length rs
  printf "#results=%d\n" n1
  let n2 = length (List.nub rs)
  when (n1 /= n2) $ do
    --mapM_ prRes ok
    printf "#results=%d #NUB=%d\n" n1 n2
    --error "*NUB*" -- TODO: would be nice to enable this

  let n = 3
  mapM_ prRes (take n ok)

  when (length bad > 0) $ do
    printf "#bad=%d\n" (length bad)
    mapM_ prRes bad
    error "*BAD*"


compile0 :: Exp -> Asm Arg
compile0 exp = do
  -- TODO: avoid splill if not in free vars ?
  perhaps spillA
  perhaps spillX
  perhaps spillY
  compile exp
  locations exp

compile :: Exp -> Asm ()
compile exp@(Exp form) = do
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

    Let x rhs body -> do
      compile rhs
      -- TODO: dont do linkage with non-determinism (alts)
      arg <- locations rhs
      let e = Exp (Var x)
      linkE e arg
      --checkFreeVarsLocatable body -- shortcut the fail.
      compile body
      loc <- locations body
      linkE exp loc


linkE :: Exp -> Arg -> Asm ()
linkE e = \case
  Imm{} -> pure()
  Loc loc -> do
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

locations :: Exp -> Asm Arg -- TODO: annoying this returns Arg not Loc !!
locations exp = do
  state <- GetSemState
  let located = locateE state exp
  locationsL exp located

locationsL :: Exp -> Located -> Asm Arg
locationsL whoExp located = do
  let tag = printf "%s--locations" (show whoExp)
  let xs = argsL located
  alts tag [ pure arg | arg <- xs ]

data Located = LocatedList [Arg]

argsL :: Located -> [Arg]
argsL (LocatedList xs) = xs

locateE :: SemState -> Exp -> Located
locateE state exp =
  LocatedList
  ((case exp of Exp (Num b) -> [Imm (Immediate b)]; _ -> []) -- TODO: surprise we need this
   ++ [ Loc loc | (loc,exps) <- Map.toList state, exp `elem` exps ])

-- TODO remove tag debugging for Alt/Nope
alts :: String -> [Asm a] -> Asm a
alts tag xs = do
  let n = length xs
  foldl (\a (i,b) -> Alt (printf "%s[%d/%d]" tag i n) a b
        ) (Nope tag) (zip [1::Int ..] xs)

----------------------------------------------------------------------
-- instruction selection

type Gen = Exp -> Form Arg -> Asm ()

select :: [Gen] -> Gen
select gs = \e f -> alts "select" [ g e f | g <- gs ]

codegen :: Gen
codegen = select [doubling,addition,xor,incrementX,incrementM]

doubling :: Gen
doubling e = \case
  Op1 Asl arg -> do loadA arg; comp e Asla
  _ -> Nope"doubling"

-- TODO: perhaps spillA after each codegen

addition :: Gen
addition e = \case
  Op2 Add (Loc RegA) arg -> do addIntoA e arg
  Op2 Add arg (Loc RegA) -> do addIntoA e arg
  Op2 Add arg1 arg2 -> do loadA arg1; addIntoA e arg2
  _ -> Nope"addition"

addIntoA :: Exp -> Arg -> Asm ()
addIntoA e = \case
  Imm imm -> do clc; comp e (Adci imm)
  Loc RegA -> comp e (Asla)
  Loc RegX -> Nope"addinto1"
  Loc RegY -> Nope"addinto2"
  Loc (ZP z) -> do clc; comp e  (Adcz z)

xor :: Gen
xor e = \case
  Op2 Xor (Loc RegA) _arg -> do eorIntoA e _arg
  Op2 Xor _arg (Loc RegA) -> do eorIntoA e _arg
  Op2 Xor arg1 arg2 -> do loadA arg1; eorIntoA e arg2
  _ -> Nope"xor"

eorIntoA :: Exp -> Arg -> Asm ()
eorIntoA e = \case
  Imm imm -> do comp e (Eori imm)
  Loc RegA -> Nope"eorIntoA1" -- or assign fixed bit pattern
  Loc RegX -> Nope"eorIntoA2"
  Loc RegY -> Nope"eorIntoA3"
  Loc (ZP z) -> do comp e (Eorz z)


incrementX :: Gen -- TODO Y like X
incrementX e = \case
  Op2 Add arg (Imm 1) -> do loadX arg; comp e Inx
  Op2 Add (Imm 1) arg -> do loadX arg; comp e Inx
  _ -> Nope"incrementX"

incrementM :: Gen
incrementM e = \case
  Op2 Add arg (Imm 1) -> do z <- inZP arg; comp e (Incz z)
  Op2 Add (Imm 1) arg -> do z <- inZP arg; comp e (Incz z)
  _ -> Nope"incrementM"

inZP :: Arg -> Asm ZeroPage
inZP = \case
  Loc (ZP z) -> pure z
  _ -> Nope"inZP"

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
  Loc RegY -> Nope"loadX(no Y->X)"
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
perhaps a = alts "perhaps" [a, pure ()] -- shorter come last; nicer for early dev/debug

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
