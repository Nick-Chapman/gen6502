module Testing (runTests) where

import Asm (runAsm,Temps(..))
import Compile (compileTarget)
import Codegen (Arg(..))
import Control.Monad (when)
import Cost (Cost)
import Emulate (EmuEnv,initMS,emulate)
import Examples (examples)
import Instruction (Code,Reg(..),ZeroPage(..),initSS)
import Language (Exp(..),EvalEnv,eval)
import Text.Printf (printf)
import qualified Cost
import qualified Data.List as List
import qualified Data.Map as Map

-- Sequence the Compilation and Asm generation of all instruction sequences.
compile :: EmuEnv -> Exp -> Reg -> IO [(Cost,Code)]
compile mu exp target = do

  let (vars,regs) = unzip (Map.toList mu)
  let (names,ss) = initSS regs
  let env = Map.fromList [ (x,Name name) | (x,name) <- zip vars names ]

  let temps = Temps [ZeroPage n | n <- [7..19]]
  xs <- runAsm Cost.lessTime temps ss (compileTarget env exp target)
  pure [ (cost,code) | (code,cost,()) <- xs ]

runTests :: IO ()
runTests = do

  -- Variables used by the examples.
  let
    a = "a"
    x = "x"
    y = "y"
    z = "z"
    z2 = "z2"

  -- Binding of variables to registers (calling convention).
  let env :: EmuEnv = Map.fromList [(a,RegA), (x,RegX), (y,RegY), (z,ZP 1), (z2,ZP 2)]

  -- Binding of variables to values (for testing of eval vs emulation).
  let ee :: EvalEnv = Map.fromList [(a,13),(x,42),(y,101),(z,19),(z2,28)]

  -- Target of compilation (calling convention).
  let target = RegA

  -- Run a test for each example:
  mapM_ (run1 target env ee) (zip [1::Int ..] examples)


run1 :: Reg -> EmuEnv -> EvalEnv -> (Int,Exp) -> IO ()
run1 target mu ee (i,example) = do

  printf "\n[%d]example = %s\n" i (show example)

  -- Evaluate the example expression
  let eres = eval ee example

  -- Compile the example; generating all instruction sequences.
  rs <- compile mu example target

  -- Error if we dont have at least one sequence.
  if length rs == 0 then error "#results==0" else pure ()

  -- Check we dont have two identical sequences (indicates inefficient codegen).
  let n1 = length rs
  --printf "#results=%d\n" n1
  let rsNubbed = List.nub rs
  let n2 = length rsNubbed
  when (n1 /= n2) $ do
    printf "#results=%d #NUB=%d\n" n1 n2
    error "*NUB*"

  -- Emulate each instruction sequence.
  let ms0 = initMS mu ee
  let rsWithEmu = [ (r, emulate ms0 code target) | r@(_,code) <- rsNubbed ]

  -- Check the emulation result matches the expected result from evaluation.
  let (ok,bad) = List.partition correct rsWithEmu
        where correct (_,mres) = (mres==eres)

  let
    prRes ((cost,code),mres) = do
      let same = (mres == eres)
      let ok :: String = if same then "" else printf " {FAIL: different: %d}" mres
      printf "{%s}: %s --> [%s]%s\n" (show cost) (show code) (show target) ok

  when (length bad > 0) $ do
    -- Show the evaluation environment & result
    printf "#bad=%d\n" (length bad)
    printf "evaluation env = %s\n" (show ee)
    printf "evaluation -> %d\n" eres
    -- to compare against the bad code sequence & emulation result
    mapM_ prRes bad
    error "*BAD*"

  -- When all is ok, show all the instruction sequences with the (same) lowest cost.
  let ((lowestCost::Cost,_),_) = head ok
  let best = takeWhile (\((cost,_),_) -> cost == lowestCost) ok
  mapM_ prRes best
