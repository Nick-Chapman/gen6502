module Testing (runTests) where

import Control.Monad (when)
import Cost (Cost)
import Data.Map (Map)
import Emulate (MachineState(..),emulate)
import Examples (examples)
import Instruction (Code)
import Language (Var,Exp(..),EvalEnv,eval,conv)
import ParserDev (CC(..),orderByCost,collectDefs,deMacro,assembleMacro)
import Semantics (Reg(..))
import Text.Printf (printf)
import Util (look)
import qualified Data.Map as Map
import qualified Program as P

type EmuEnv = Map Var Reg

-- Sequence the Compilation and Asm generation of all instruction sequences.

compile :: Exp -> IO [(Cost,Code)]
compile example = do
  -- TODO: take formals and (reg)args from a passed env
  let entryName = "example"
  let formals = ["a","x","y","z","z2"]
  -- TODO: build program def direct, rather than going via old exp
  let def = P.Def entryName formals (conv example)
  let prog = P.Prog [def]
  let progEnv = collectDefs prog
  let entry = deMacro (look "run1" progEnv entryName)
  let target = RegA
  let args = [RegA,RegX,RegY,ZP 1,ZP 2]
  let cc = CC { args, target }
  xs <- assembleMacro entry cc
  pure (orderByCost xs)


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
  rs <- compile example

  printf "#results=%d\n" (length rs)

  -- Error if we dont have at least one sequence.
  if length rs == 0 then error "#results==0" else pure ()

  -- Collect the sequences withthe lowest cose
  let (lowestCost::Cost,_) = head rs
  let best = takeWhile (\(cost,_) -> cost == lowestCost) rs

  -- setup initial machine-state for emultaion
  let regs = Map.fromList [ (loc,look "initMS" ee var) | (var,loc) <- Map.toList mu ]
  let flags = Map.empty
  let ms0 = MS { regs, flags }

  -- Emulate & display an instruction sequence...
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
