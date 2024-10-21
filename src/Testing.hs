module Testing (runTests) where

import Asm (runAsm,AsmState(..))
import Codegen (Arg(..))
import Compile (compileTarget)
import Control.Monad (when)
import Cost (Cost,costOfCode,lessTime)
import Data.List (sortBy)
import Emulate (EmuEnv,initMS,emulate)
import Examples (examples)
import Instruction (Code)
import Language (Exp(..),EvalEnv,eval)
import Semantics (Reg(..),ZeroPage(..),initSS)
import Text.Printf (printf)
import qualified Data.Map as Map

-- Sequence the Compilation and Asm generation of all instruction sequences.
compile :: EmuEnv -> Exp -> Reg -> [(Cost,Code)]
compile mu exp target = do
  let (vars,regs) = unzip (Map.toList mu)
  let (names,ss) = initSS regs
  let env = Map.fromList [ (x,Name name) | (x,name) <- zip vars names ]
  let temps = [ZeroPage n | n <- [7..19]]
  let asm = compileTarget env exp target
  let state = AsmState { ss, temps }
  let xs = runAsm state asm
  orderByCost xs

-- TODO: produce results in cost order, rather than post-sorting
orderByCost :: [Code] -> [(Cost,Code)]
orderByCost xs = do
  sortByCost [ (costOfCode code, code) | code <- xs ]
  where
    sortByCost =
      sortBy (\(c1,code1) (c2,code2) ->
                 case lessTime c1 c2 of
                   EQ -> compare code1 code2 -- order determinism of tests
                   x -> x)

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
  let rs = compile mu example target
  --printf "#results=%d\n" (length rs)

  -- Error if we dont have at least one sequence.
  if length rs == 0 then error "#results==0" else pure ()

  -- Collect the sequences withthe lowest cose
  let (lowestCost::Cost,_) = head rs
  let best = takeWhile (\(cost,_) -> cost == lowestCost) rs

  -- Emulate & display an instruction sequence...
  let ms0 = initMS mu ee
  let
    prRes see (cost,code) = do
      let mres = emulate ms0 code target
      let same = (mres == eres)
      when (see || not same) $ do
        let ok :: String = if same then "" else printf " {FAIL: different: %d}" mres
        printf "{%s}: %s --> [%s]%s\n" (show cost) (show code) (show target) ok
      when (not same) $ do
        printf "evaluation env = %s\n" (show ee)
        printf "evaluation -> %d\n" eres
        error "*BAD*"

  -- Emulate/check/display the best sequences
  mapM_ (prRes True) best

  -- Emulate/check all sequences
  mapM_ (prRes False) rs
