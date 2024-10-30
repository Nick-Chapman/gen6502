module Testing (runTests) where

import Control.Monad (when)
import Cost (Cost,orderByCost)
import Emulate (MachineState(..),emulate)
import Examples (examples,Trip(..))
import Par4 (parse)
import Compile (CC(..),Macro(..),collectDefs,deMacro,assembleMacro)
import Program (Def(..),exec,Value(VNum),gram6)
import Text.Printf (printf)
import Util (look)
import qualified Data.Map as Map

runTests :: IO ()
runTests = do
  prog <- parse gram6 <$> readFile "examples/first.ml6"
  mapM_ run1 (zip [1::Int ..] (examples prog))

run1 :: (Int,Examples.Trip) -> IO ()
run1 (i,example) = do

  let Trip{prog,entryName,cc} = example
  let CC { args, target } = cc

  let progEnv = collectDefs prog
  let entry = deMacro (look "run1" progEnv entryName)
  let Macro{def} = entry
  let Def{formals,body} = def
  printf "\n[%d]example = %s\n" i (show body)

  -- Evaluate the example
  let argBytes = take (length formals) [13,42,101,19,28]
  let eres = exec prog entryName (map VNum argBytes)

  -- Compile the example; generating all instruction sequences.
  xs <- assembleMacro entry cc
  let rs = orderByCost xs

  printf "#results=%d\n" (length rs)

  -- Error if we dont have at least one sequence.
  if length rs == 0 then error "#results==0" else pure ()

  -- Collect the sequences withthe lowest cose
  let (lowestCost::Cost,_) = head rs
  let best = takeWhile (\(cost,_) -> cost == lowestCost) rs

  -- setup initial machine-state for emulation

  let regs = Map.fromList (zip args argBytes)
  let flags = Map.empty
  let ms0 = MS { regs, flags }

  -- Emulate & display an instruction sequence...
  let
    prRes see (cost,code) = do
      let mres = emulate ms0 code target
      let same = (VNum mres == eres)
      when (see || not same) $ do
        let ok :: String = if same then "" else printf " {FAIL: different: %d}" mres
        printf "{%s}: %s --> [%s]%s\n" (show cost) (show code) (show target) ok
      when (not same) $ do
        printf "evaluation env = %s\n" (show (zip formals argBytes))
        printf "evaluation -> %s\n" (show eres)
        error "*BAD*"

  -- Emulate/check/display the best sequences
  mapM_ (prRes True) best

  -- Emulate/check all sequences
  mapM_ (prRes False) rs
