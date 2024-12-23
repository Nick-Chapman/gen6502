module Semantics
  ( clc, sec, trans, compute, compare
  ) where

import Prelude hiding (compare)

import Architecture (Immediate,Reg(..))
import Asm (Asm,freshName,freshName1,emitWithSemantics)
import Instruction (Instruction(..),ITransfer(..),ICompute(..),ICompare(..))
import SemState (Name(..),Name1(..),Arg(..),Arg1(..),SemState,lookSS,updateSS)
import Text.Printf (printf)

----------------------------------------------------------------------
-- Semantics (function over SemState)

type Semantics = SemState -> SemState

noSemantics :: Semantics
noSemantics = id

transfer :: Reg -> Reg -> Semantics
transfer src dest = \s -> updateSS dest (lookSS tag src s) s
  where tag = printf "transfer:%s-->%s" (show src) (show dest)

overwrite :: Name -> Reg -> Semantics
overwrite name reg ss = updateSS reg (Name name) ss

overwriteI :: Immediate -> Reg -> Semantics
overwriteI imm reg ss = updateSS reg (Imm imm) ss

----------------------------------------------------------------------
-- instruction semantics

transferSemantics :: ITransfer -> Semantics
transferSemantics = \case
  Tax -> transfer RegA RegX
  Tay -> transfer RegA RegY
  Txa -> transfer RegX RegA
  Tya -> transfer RegY RegA
  Ldai i -> overwriteI i RegA
  Ldxi i -> overwriteI i RegX
  Ldyi i -> overwriteI i RegY
  Ldaz z -> transfer (ZP z) RegA
  Ldxz z -> transfer (ZP z) RegX
  Ldyz z -> transfer (ZP z) RegY
  Sta z -> transfer RegA (ZP z)
  Stx z -> transfer RegX (ZP z)
  Sty z -> transfer RegY (ZP z)

computeSemantics :: Name -> ICompute -> Semantics
computeSemantics name = \case
  Adci{} -> overwrite name RegA
  Adcz{} -> overwrite name RegA
  Sbci{} -> overwrite name RegA
  Sbcz{} -> overwrite name RegA
  Andi{} -> overwrite name RegA
  Andz{} -> overwrite name RegA
  Eori{} -> overwrite name RegA
  Eorz{} -> overwrite name RegA
  Inx -> overwrite name RegX
  Iny -> overwrite name RegY
  Incz z -> overwrite name (ZP z)
  Asla -> overwrite name RegA
  Aslz z -> overwrite name (ZP z)
  Lsra -> overwrite name RegA
  Lsrz z -> overwrite name (ZP z)

compareSemantics :: Name1 -> ICompare -> Semantics
compareSemantics _name = \case -- TODO
  Cmpz{} -> noSemantics
  Cmpi{} -> noSemantics

----------------------------------------------------------------------
-- emitWithSemantics

-- TODO: track carry flag
clc :: Asm ()
clc = emitWithSemantics Clc noSemantics

sec :: Asm ()
sec = emitWithSemantics Sec noSemantics

trans :: ITransfer -> Asm ()
trans i = emitWithSemantics (Tx i) (transferSemantics i)

compute :: ICompute -> Asm Arg
compute i = do
  name <- freshName
  emitWithSemantics (Compute i) (computeSemantics name i)
  pure (Name name)

compare :: ICompare -> Asm Arg1
compare i = do
  name1 <- freshName1 -- TODO: name1 should be different type to name??
  emitWithSemantics (Compare i) (compareSemantics name1 i)
  pure (Name1 name1) -- oh, and not just here
