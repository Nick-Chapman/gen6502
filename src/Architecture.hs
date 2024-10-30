
module Architecture
  ( Immediate(..), ZeroPage(..), Reg(..), Flag(..)
  ) where

import Data.Word (Word8)

type Byte = Word8

----------------------------------------------------------------------
-- ZeroPage & Immediate

newtype ZeroPage = ZeroPage Byte
  deriving (Eq,Ord,Num)

newtype Immediate = Immediate Byte
  deriving (Eq,Ord,Num)

instance Show ZeroPage where
  show (ZeroPage z) = show z

instance Show Immediate where
  show (Immediate b) = "#" ++ show b

----------------------------------------------------------------------
-- Reg/Flag

data Reg = RegA | RegX | RegY | ZP ZeroPage
  deriving (Eq,Ord)

-- 1bit equivalent of Reg
data Flag = FlagZ | FlagN -- TODO: need flags for N/C
  deriving (Eq,Ord)

instance Show Reg where
  show = \case
    RegA -> "A"
    RegX -> "X"
    RegY -> "Y"
    ZP z -> "ZP-" ++ show z

instance Show Flag where
  show = \case
    FlagZ -> "z"
    FlagN -> "n"
