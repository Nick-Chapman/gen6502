module Is5 (main) where

import Asm (Asm(..),runAsm,Temps(..))
import Control.Monad (when)
import Emulate (Env,MachineState,initMS,emulate)
import Instruction (Instruction(..),ITransfer(..),ICompute(..),Loc(..),ZeroPage(..),Immediate(..),SemState,noSemantics,transferSemantics,computeSemantics)
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

  let a = var "a"
  let x = var "x"
  let y = var "y"
  let z = var "z"
  let z2 = var "z2"

  let
    examples =
      [ num 77

      , a
      , x
      , y
      , z

      , add a (num 1)
      , add x (num 1)
      , add y (num 1)
      , add z (num 1)

      , add (num 1) a
      , add (num 1) x
      , add (num 1) y
      , add (num 1) z

      , add a (num 2)
      , add x (num 2)
      , add y (num 2)
      , add z (num 2)

      , add (num 3) a
      , add (num 3) x
      , add (num 3) y
      , add (num 3) z

      , add a a
      , add a x
      , add a y
      , add a z

      , add x a
      , add x x
      , add x y
      , add x z

      , add y a
      , add y x
      , add y y
      , add y z

      , add z a
      , add z x
      , add z y
      , add z z

      , add z z2
      , add (num 17) (num 19)
      , add (num 14) (num 1)
      , add (num 1) (num 14)

      , asl (num 14)
      , asl a
      , asl x
      , asl y
      , asl z

      , add (asl a) a
      , add (asl x) x
      , add (asl y) y
      , add (asl z) z

      , add (add a (num 1)) (add a (num 1))
      , add (add (num 17) (num 19)) (add (num 17) (num 19))

      , xor a (asl a)
      , xor a (asl (asl a))

      , xor (add a (num 1)) (add a (num 1))
      , xor (add x (num 1)) (add x (num 1))
      , xor (add y (num 1)) (add y (num 1))
      , xor (add z (num 1)) (add z (num 1))

      , xor (xor a (asl a)) (asl (asl a))


      , let_ "t1" a (var "t1")
      , let_ "t1" x (var "t1")
      , let_ "t1" y (var "t1")
      , let_ "t1" z (var "t1")
      , let_ "t1" a (add (var "t1") (var "t1"))
      , let_ "t1" x (add (var "t1") (var "t1"))
      , let_ "t1" y (add (var "t1") (var "t1"))
      , let_ "t1" z (add (var "t1") (var "t1"))
      , let_ "t1" (asl (add (num 1) z)) (add z2 (var "t1"))

      -- , var "b" -- should fail because no in env, rather than no final location

      , xor (xor (asl x) (asl y)) (xor (asl z) (asl a))

      , add (add (add a x) (add y z)) (add (add x y) (add z a))
      , xor (xor (xor a x) (xor y z)) (xor (xor x y) (xor z a))

      ]

  printf "(eval)env = %s\n" (show ee)
  let ms0 :: MachineState = initMS env ee
  printf "ms0 = %s\n" (show ms0)

  let ss = semStateOfEnv env
  mapM_ (run1 ss ee ms0) (last (1000) (zip [1::Int ..] examples))
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

  let n1 = length rs
  printf "#results=%d\n" n1
  let rsNubbed = List.nub rs
  let n2 = length rsNubbed
  when (n1 /= n2) $ do
    --mapM_ prRes ok
    printf "#results=%d #NUB=%d\n" n1 n2
    error "*NUB*"

  let
    a_emulate code = \case
      Imm (Immediate b) -> b
      MLoc located ->
        case somewhere located of
          Just loc -> emulate ms0 code loc
          Nothing -> error "no final location"

  let rsWithEmu = [ (r, a_emulate code final) | r@(code,_,final) <- rsNubbed ]
  let (ok,bad) = List.partition correct rsWithEmu
        where correct (_,mres) = (mres==eres)

  let n = 3
  mapM_ prRes (take n ok)

  when (length bad > 0) $ do
    printf "eval -> %d\n" eres

  when (length bad > 0) $ do
    printf "#bad=%d\n" (length bad)
    mapM_ prRes bad
    error "*BAD*"

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



data Arg = Imm Immediate | MLoc Located deriving (Eq)

instance Show Arg where
  show = \case
    Imm imm -> show imm
    MLoc located  -> show located


----------------------------------------------------------------------
-- Located

data Located = Located { a,x,y::Bool,z::Maybe ZeroPage } deriving (Eq)

instance Show Located where
  show located = show (everywhere located)

everywhere :: Located -> [Loc]
everywhere Located{a,x,y,z} = do
  concat
    [ if a then [RegA] else []
    , if x then [RegX] else []
    , if y then [RegY] else []
    , case z of Just z -> [ZP z]; Nothing -> []
    ]

somewhere :: Located -> Maybe Loc
somewhere Located{a,x,y,z} = do
  if a then Just RegA else
    if x then Just RegX else
      if y then Just RegY else
        case z of
          Just z -> Just (ZP z)
          Nothing -> Nothing

classifyLocs :: [Loc] -> Located
classifyLocs xs = do
  let nowhere = Located { a = False, x = False, y = False, z = Nothing }
  let
    f acc = \case
      RegA -> acc { a = True }
      RegX -> acc { x = True }
      RegY -> acc { y = True }
      ZP z -> acc { z = Just z }
  foldl f nowhere xs

inAcc :: Arg -> Bool
inAcc = \case
  Imm{} -> False
  MLoc Located{a} -> a

canOp :: Arg -> Bool
canOp = \case
  Imm _ -> True
  MLoc Located{z=Just{}} -> True
  MLoc{} -> False


locations :: Exp -> Asm Arg
locations = \case
  Exp (Num b) -> pure (Imm (Immediate b))
  exp -> do
    state <- GetSemState
    let xs = [ loc | (loc,exps) <- Map.toList state, exp `elem` exps ]
    pure (MLoc (classifyLocs xs))

alts :: [Asm a] -> Asm a
alts = \case
  [] -> Nope
  x:xs -> foldl Alt x xs

----------------------------------------------------------------------
-- instruction selection

type Gen = Exp -> Form Arg -> Asm ()

select :: [Gen] -> Gen
select gs = \e f -> alts [ g e f | g <- gs ]

codegen :: Gen
codegen = select [ driveA, driveX, driveY, driveZ ]

-- TODO driveY
driveA,driveX,driveY,driveZ :: Gen
driveA = maybePostSpillA $ select [doublingA,addition,xor]
driveX = maybePostSpillX $ select [incrementX]
driveY = maybePostSpillY $ select [incrementY]
driveZ = select [incrementZ,doublingZ]

doublingA :: Gen
doublingA = \e ->  \case
  Op1 Asl arg -> do loadA arg; comp e Asla
  _ -> Nope

addition :: Gen
addition = \e -> \case
  Op2 Add arg1 arg2 ->
    if inAcc arg1 && inAcc arg2
    then comp e (Asla)
    else
      if inAcc arg1 then addIntoA e arg2 else
        if inAcc arg2 then addIntoA e arg1 else
          if canOp arg2
          then do loadA arg1; addIntoA e arg2
          else do loadA arg2; addIntoA e arg1 -- swap
  _ ->
    Nope

addIntoA :: Exp -> Arg -> Asm ()
addIntoA e = \case
  Imm imm -> do clc; comp e (Adci imm)
  MLoc Located{z} ->
    -- only location: Z (not A,X,Y)
    case z of
      Just z -> do clc; comp e (Adcz z)
      Nothing -> Nope

xor :: Gen
xor = \e -> \case
  Op2 Xor arg1 arg2 ->
    if inAcc arg1 then eorIntoA e arg2 else
      if inAcc arg2 then eorIntoA e arg1 else
        if canOp arg2
        then do loadA arg1; eorIntoA e arg2
        else do loadA arg2; eorIntoA e arg1 -- swap
  _ ->
    Nope

eorIntoA :: Exp -> Arg -> Asm ()
eorIntoA e = \case
  Imm imm -> do comp e (Eori imm)
  MLoc Located{z} ->
    -- only location: Z (not A,X,Y)
    case z of
      Just z -> do comp e (Eorz z)
      Nothing -> Nope

incrementX :: Gen
incrementX = \e -> \case
  Op2 Add arg (Imm 1) -> do loadX arg; comp e Inx
  Op2 Add (Imm 1) arg -> do loadX arg; comp e Inx
  _ -> Nope

incrementY :: Gen
incrementY = \e -> \case
  Op2 Add arg (Imm 1) -> do loadY arg; comp e Iny
  Op2 Add (Imm 1) arg -> do loadY arg; comp e Iny
  _ -> Nope

incrementZ :: Gen
incrementZ e = \case
  Op2 Add arg (Imm 1) -> do z <- inZP arg; comp e (Incz z)
  Op2 Add (Imm 1) arg -> do z <- inZP arg; comp e (Incz z)
  _ -> Nope

doublingZ :: Gen
doublingZ = \e ->  \case
  Op1 Asl arg -> do z <- inZP arg; comp e (Aslz z)
  _ -> Nope

inZP :: Arg -> Asm ZeroPage
inZP = \case
  Imm{} -> Nope
  MLoc Located{z} ->
    case z of
      Just z -> pure z
      Nothing -> Nope

-- TODO: compile time constant folding

----------------------------------------------------------------------
-- codegen helpers

loadA :: Arg -> Asm ()
loadA = \case
  Imm imm -> trans (Ldai imm)
  MLoc Located{a,x,y,z} ->
    -- prefer location: A,X,Y,Z
    if a then pure () else do
      if x then trans Txa else do
        if y then trans Tya else do
          case z of
            Just z -> trans (Ldaz z);
            Nothing -> Nope


loadX :: Arg -> Asm ()
loadX = \case
  Imm imm -> trans (Ldxi imm)
  MLoc Located{a,x,y,z} ->
    -- prefer location: X,A,Z (not Y)
    if x then pure () else do
      if a then trans Tax else
        case z of
          Just z -> trans (Ldxz z);
          Nothing ->
            if y then Nope else
              error "must be Located somewhere" -- is this true?
              -- Nope -- instead just use Nope

loadY :: Arg -> Asm ()
loadY = \case
  Imm imm -> trans (Ldyi imm)
  MLoc Located{a,x,y,z} ->
    -- prefer location: Y,A,Z (not X)
    if y then pure () else do
      if a then trans Tay else
        case z of
          Just z -> trans (Ldyz z);
          Nothing ->
            if x then Nope else
              error "must be Located somewhere" -- is this true?
              -- Nope -- instead just use Nope

clc :: Asm ()
clc = Emit Clc noSemantics

trans :: ITransfer -> Asm ()
trans i = Emit (Tx i) (transferSemantics i)

comp :: Exp -> ICompute -> Asm ()
comp e i = Emit (Comp i) (computeSemantics e i)


----------------------------------------------------------------------
-- spilling...

perhaps :: Asm () -> Asm ()
perhaps a = alts [a, pure ()]

-- TODO: only spill if these location map to some expression
-- TODO: better to split just before overwrite?

maybePostSpillA :: Gen -> Gen
maybePostSpillA g e f = Alt (g e f) (do g e f; spillA)

maybePostSpillX :: Gen -> Gen
maybePostSpillX g e f = Alt (g e f) (do g e f; spillX)

maybePostSpillY :: Gen -> Gen
maybePostSpillY g e f = Alt (g e f) (do g e f; spillY)

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
