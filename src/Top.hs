module Top where

import qualified Testing

main :: IO ()
main = do
  putStrLn "*gen6502*"
  Testing.runTests
