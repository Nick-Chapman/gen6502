module Is5 (main) where

import Asm (runAsm,Temps(..))
import Codegen (Arg(..),somewhere)
import Compile (compile0)
import Control.Monad (when)
import Emulate (Env,MachineState,initMS,emulate)
import Instruction (Loc(..),ZeroPage(..),Immediate(..),SemState)
import Language (Exp(..),Form(..),Op1(..),Op2(..),EvalEnv,eval)
import Text.Printf (printf)
import qualified Cost
import qualified Data.List as List
import qualified Data.Map as Map

-- TODO: split out modules for testing

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
  let sub e1 e2 = Exp (Op2 Sub e1 e2)
  let xor e1 e2 = Exp (Op2 Xor e1 e2)
  let asl e = Exp (Op1 Asl e)

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


      , sub a (num 1)
      , sub x (num 1)
      , sub y (num 1)
      , sub z (num 1)
      , sub (num 1) a
      , sub (num 1) x
      , sub (num 1) y
      , sub (num 1) z
      , sub a (num 2)
      , sub x (num 2)
      , sub y (num 2)
      , sub z (num 2)
      , sub (num 3) a
      , sub (num 3) x
      , sub (num 3) y
      , sub (num 3) z
      , sub a a
      , sub a x
      , sub a y
      , sub a z
      , sub x a
      , sub x x
      , sub x y
      , sub x z
      , sub y a
      , sub y x
      , sub y y
      , sub y z
      , sub z a
      , sub z x
      , sub z y
      , sub z z
      , sub z z2
      , sub (num 17) (num 19)
      , sub (num 14) (num 1)
      , sub (num 1) (num 14)



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
