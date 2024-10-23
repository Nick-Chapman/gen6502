module Top where

import System.Environment (getArgs)
import qualified Testing
import qualified ParserDev

main :: IO ()
main = do
  putStrLn "*gen6502*"
  args <- getArgs
  case (parseCommandLine args) of
    RunTests -> Testing.runTests
    ParserDev file -> ParserDev.main file

data Mode = RunTests | ParserDev FilePath

parseCommandLine :: [String] -> Mode
parseCommandLine = \case
  ["test"] -> RunTests
  ["dev",file] -> ParserDev file
  [] -> do
    let file = "examples/collatz.ml6"
    ParserDev file
  xs -> error (show ("parseCommandLine",xs))
