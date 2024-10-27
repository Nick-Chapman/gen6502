module Util
  ( look, extend, zipCheck
  ) where

import Data.Map (Map)
import qualified Data.Map as Map

look :: (Ord k, Show k) => String -> Map k v -> k -> v
look tag m k = maybe err id (Map.lookup k m) where err = error (show ("look",tag,k))

extend :: Ord k => Map k v -> k -> v -> Map k v
extend m k v = Map.insert k v m

zipCheck :: String -> [a] -> [b] -> [(a,b)]
zipCheck tag xs ys =
  if length xs /= length ys then error (show ("zipCheck",tag,length xs,length ys)) else
    zip xs ys
