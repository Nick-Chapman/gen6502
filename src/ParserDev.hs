module ParserDev (main,CC(..),Macro(..),collectDefs,deMacro,assembleMacro) where

import Architecture (Reg(..))
import Compile (CC(..),Macro(..),collectDefs,deMacro,assembleMacro)
import Control.Monad (when)
import Cost (Cost,orderByCost)
import Data.Word (Word8)
import Emulate (MachineState(..),emulate)
import Instruction (Code)
import Par4 (parse)
import Program (Prog(..),Def(..),Id,gram6,exec,Value(..))
import Text.Printf (printf)
import Util (look,zipCheck)
import qualified Data.Map as Map

type Byte = Word8

main :: String -> IO ()
main entryName = do
  s <- readFile "examples/examples.ml6"
  let prog = parse gram6 s
  --print prog
  go prog entryName

go :: Prog -> Id -> IO ()
go prog entryName = do

  printf "\n**[%s]**\n" entryName
  let env = collectDefs prog
  let entry = deMacro (look "go" env entryName)
  let cc = pickCC entry

  -- evaluate/emulate for specific argument values
  let argBytes = take (numberOfArgs entry) [7::Byte .. ]
  let eres = exec prog entryName (map VNum argBytes)
  printf "evaluation -> %s\n" (show eres)

  alts <- assembleMacro entry cc
  printf "#%d alts\n" (length alts)
  checkCode eres cc argBytes alts


macroFormals :: Macro -> [Id]
macroFormals Macro { def = Def { formals } } = formals

numberOfArgs :: Macro -> Int
numberOfArgs m = length (macroFormals m)


pickCC :: Macro -> CC
pickCC m = do
  let target = RegA
  let args = take (numberOfArgs m) [RegA,RegX,RegY,ZP 0,ZP 1] -- TODO: might need more!
  CC { args, target }


checkCode :: Value -> CC -> [Byte] -> [Code] -> IO ()
checkCode eres cc argBytes alts = do
  let all = orderByCost alts
  _best <- selectCodeAlt all
  let CC { args = argRegs, target = targetReg } = cc
  let regs = Map.fromList (zipCheck "setup-emu-env" argRegs argBytes)
  let ms0 = MS { regs, flags = Map.empty }
  let
    tryCode (cost,code) = do
      printf "%s: %s\n" (show cost) (show code)
      let mres = emulate ms0 code targetReg
      let same = (VNum mres == eres)
      when (not same) $ do
        printf "emulation -> %s\n" (show mres)
        printf "*DIFF*\n"
      pure ()
  --mapM_ tryCode [ head _best ]
  mapM_ tryCode _best

selectCodeAlt :: [(Cost,Code)] -> IO [(Cost,Code)]
selectCodeAlt ys = do
  case ys of
    [] -> error "no alts"
    (lowestCost,_):_ -> do
      let best = takeWhile (\(cost,_) -> cost == lowestCost) ys
      printf "smallest cost = %s, from #%d alternatives\n" (show lowestCost) (length best)
      pure best
