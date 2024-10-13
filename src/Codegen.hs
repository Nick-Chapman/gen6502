module Codegen
  ( Arg(..)
  , locations
  , somewhere, everywhere
  , perhaps, spillA, spillX, spillY
  , codegen
  , alts
  ) where

import Asm (Asm(..))
import Instruction (Instruction(..),ITransfer(..),ICompute(..),Loc(..),ZeroPage(..),Immediate(..),noSemantics,transferSemantics,computeSemantics)
import Language (Exp(..),Form(..),Op2(..),Op1(..))
import qualified Data.Map as Map

type Gen = Exp -> Form Arg -> Asm ()

----------------------------------------------------------------------
-- instruction selection

codegen :: Gen
codegen = select [ driveA, driveX, driveY, driveZ ]

select :: [Gen] -> Gen
select gs = \e f -> alts [ g e f | g <- gs ]

driveA,driveX,driveY,driveZ :: Gen
driveA = maybePostSpillA $ select [doublingA,addition,xor]
driveX = maybePostSpillX $ select [incrementX]
driveY = maybePostSpillY $ select [incrementY]
driveZ = select [incrementZ,doublingZ]

doublingA :: Gen
doublingA = \e ->  \case
  Op1 Asl arg -> do loadA arg; comp e Asla
  _ -> Nope

addition :: Gen
addition = \e -> \case
  Op2 Add arg1 arg2 ->
    if inAcc arg1 && inAcc arg2
    then comp e (Asla)
    else
      if inAcc arg1 then addIntoA e arg2 else
        if inAcc arg2 then addIntoA e arg1 else
          if canOp arg2
          then do loadA arg1; addIntoA e arg2
          else do loadA arg2; addIntoA e arg1 -- swap
  _ ->
    Nope

addIntoA :: Exp -> Arg -> Asm ()
addIntoA e = \case
  Imm imm -> do clc; comp e (Adci imm)
  MLoc Located{z} ->
    -- only location: Z (not A,X,Y)
    case z of
      Just z -> do clc; comp e (Adcz z)
      Nothing -> Nope

xor :: Gen
xor = \e -> \case
  Op2 Xor arg1 arg2 ->
    if inAcc arg1 then eorIntoA e arg2 else
      if inAcc arg2 then eorIntoA e arg1 else
        if canOp arg2
        then do loadA arg1; eorIntoA e arg2
        else do loadA arg2; eorIntoA e arg1 -- swap
  _ ->
    Nope

eorIntoA :: Exp -> Arg -> Asm ()
eorIntoA e = \case
  Imm imm -> do comp e (Eori imm)
  MLoc Located{z} ->
    -- only location: Z (not A,X,Y)
    case z of
      Just z -> do comp e (Eorz z)
      Nothing -> Nope

incrementX :: Gen
incrementX = \e -> \case
  Op2 Add arg (Imm 1) -> do loadX arg; comp e Inx
  Op2 Add (Imm 1) arg -> do loadX arg; comp e Inx
  _ -> Nope

incrementY :: Gen
incrementY = \e -> \case
  Op2 Add arg (Imm 1) -> do loadY arg; comp e Iny
  Op2 Add (Imm 1) arg -> do loadY arg; comp e Iny
  _ -> Nope

incrementZ :: Gen
incrementZ e = \case
  Op2 Add arg (Imm 1) -> do z <- inZP arg; comp e (Incz z)
  Op2 Add (Imm 1) arg -> do z <- inZP arg; comp e (Incz z)
  _ -> Nope

doublingZ :: Gen
doublingZ = \e ->  \case
  Op1 Asl arg -> do z <- inZP arg; comp e (Aslz z)
  _ -> Nope

inZP :: Arg -> Asm ZeroPage
inZP = \case
  Imm{} -> Nope
  MLoc Located{z} ->
    case z of
      Just z -> pure z
      Nothing -> Nope

-- TODO: compile time constant folding

loadA :: Arg -> Asm ()
loadA = \case
  Imm imm -> trans (Ldai imm)
  MLoc Located{a,x,y,z} ->
    -- prefer location: A,X,Y,Z
    if a then pure () else do
      if x then trans Txa else do
        if y then trans Tya else do
          case z of
            Just z -> trans (Ldaz z);
            Nothing -> Nope

loadX :: Arg -> Asm ()
loadX = \case
  Imm imm -> trans (Ldxi imm)
  MLoc Located{a,x,y,z} ->
    -- prefer location: X,A,Z (not Y)
    if x then pure () else do
      if a then trans Tax else
        case z of
          Just z -> trans (Ldxz z);
          Nothing ->
            if y then Nope else
              error "must be Located somewhere" -- is this true?
              -- Nope -- instead just use Nope

loadY :: Arg -> Asm ()
loadY = \case
  Imm imm -> trans (Ldyi imm)
  MLoc Located{a,x,y,z} ->
    -- prefer location: Y,A,Z (not X)
    if y then pure () else do
      if a then trans Tay else
        case z of
          Just z -> trans (Ldyz z);
          Nothing ->
            if x then Nope else
              error "must be Located somewhere" -- is this true?
              -- Nope -- instead just use Nope

clc :: Asm ()
clc = Emit Clc noSemantics

----------------------------------------------------------------------
-- spilling...

perhaps :: Asm () -> Asm ()
perhaps a = alts [a, pure ()]

-- TODO: only spill if these location map to some expression
-- TODO: better to split just before overwrite?

maybePostSpillA :: Gen -> Gen
maybePostSpillA g e f = Alt (g e f) (do g e f; spillA)

maybePostSpillX :: Gen -> Gen
maybePostSpillX g e f = Alt (g e f) (do g e f; spillX)

maybePostSpillY :: Gen -> Gen
maybePostSpillY g e f = Alt (g e f) (do g e f; spillY)

spillA :: Asm ()
spillA = do
  z <- Fresh
  trans (Sta z)

spillX :: Asm ()
spillX = do
  z <- Fresh
  trans (Stx z)

spillY :: Asm ()
spillY = do
  z <- Fresh
  trans (Sty z)

trans :: ITransfer -> Asm ()
trans i = Emit (Tx i) (transferSemantics i)

comp :: Exp -> ICompute -> Asm ()
comp e i = Emit (Comp i) (computeSemantics e i)

alts :: [Asm a] -> Asm a
alts = \case
  [] -> Nope
  x:xs -> foldl Alt x xs

----------------------------------------------------------------------
-- Arg

data Arg = Imm Immediate | MLoc Located deriving (Eq)

instance Show Arg where
  show = \case
    Imm imm -> show imm
    MLoc located  -> show located

locations :: Exp -> Asm Arg
locations = \case
  Exp (Num b) -> pure (Imm (Immediate b))
  exp -> do
    state <- GetSemState
    let xs = [ loc | (loc,exps) <- Map.toList state, exp `elem` exps ]
    pure (MLoc (classifyLocs xs))

----------------------------------------------------------------------
-- Located

data Located = Located { a,x,y::Bool,z::Maybe ZeroPage } deriving (Eq)

instance Show Located where
  show located = show (everywhere located)

everywhere :: Located -> [Loc]
everywhere Located{a,x,y,z} = do
  concat
    [ if a then [RegA] else []
    , if x then [RegX] else []
    , if y then [RegY] else []
    , case z of Just z -> [ZP z]; Nothing -> []
    ]

somewhere :: Located -> Maybe Loc
somewhere Located{a,x,y,z} = do
  if a then Just RegA else
    if x then Just RegX else
      if y then Just RegY else
        case z of
          Just z -> Just (ZP z)
          Nothing -> Nothing

classifyLocs :: [Loc] -> Located
classifyLocs xs = do
  let nowhere = Located { a = False, x = False, y = False, z = Nothing }
  let
    f acc = \case
      RegA -> acc { a = True }
      RegX -> acc { x = True }
      RegY -> acc { y = True }
      ZP z -> acc { z = Just z }
  foldl f nowhere xs

inAcc :: Arg -> Bool
inAcc = \case
  Imm{} -> False
  MLoc Located{a} -> a

canOp :: Arg -> Bool
canOp = \case
  Imm _ -> True
  MLoc Located{z=Just{}} -> True
  MLoc{} -> False
