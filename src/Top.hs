module Top where

import System.Environment (getArgs)
import qualified Testing
import qualified ParserDev

main :: IO ()
main = do
  args <- getArgs
  case (parseCommandLine args) of
    RunTests -> Testing.runTests
    ParserDev entryName -> ParserDev.main entryName

data Mode = RunTests | ParserDev String

parseCommandLine :: [String] -> Mode
parseCommandLine = \case
  [] -> RunTests
  [entryName] -> ParserDev entryName
  xs -> error (show ("parseCommandLine",xs))
