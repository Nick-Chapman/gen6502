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
    ParserDev -> ParserDev.main

data Mode = RunTests | ParserDev

parseCommandLine :: [String] -> Mode
parseCommandLine = \case
  ["test"] -> RunTests
  ["dev"] -> ParserDev
  [] -> ParserDev
  xs -> error (show ("parseCommandLine",xs))
