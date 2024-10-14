module Codegen
  ( preamble, codegen, locations, assign, Reg
  ) where

import Asm (Asm(..))
import Instruction (Instruction(..),ITransfer(..),ICompute(..),Reg(..),ZeroPage(..),Immediate(..),noSemantics,transferSemantics,computeSemantics)
import Language (Exp(..),Form(..),Op2(..),Op1(..))
import Text.Printf (printf)

----------------------------------------------------------------------
-- Arg

data Arg = Imm Immediate | Loc Located deriving (Eq)

data Located = Located { a,x,y::Bool,z::Maybe ZeroPage } deriving (Eq)

instance Show Located where
  show located = show (everywhere located)

instance Show Arg where
  show = \case
    Imm imm -> show imm
    Loc located  -> show located

----------------------------------------------------------------------
-- instruction selection and code generation

type Gen = Exp -> Form Arg -> Asm ()

preamble :: Asm ()
preamble = do
  perhaps spillA
  perhaps spillX
  perhaps spillY

codegen :: Gen
codegen = select
  [ do driveA `conclude` perhaps spillA
  , do driveX `conclude` perhaps spillX
  , do driveY `conclude` perhaps spillY
  , do driveZ
  ]

perhaps :: Asm () -> Asm ()
perhaps a = Alt (pure ()) a

driveA,driveX,driveY,driveZ :: Gen
driveA = select [doublingA,addition,subtraction,xor]
driveX = select [incrementX]
driveY = select [incrementY]
driveZ = select [incrementZ,doublingZ]

select :: [Gen] -> Gen
select gs = \e f -> alternatives [ g e f | g <- gs ]

conclude :: Gen -> Asm () -> Gen
conclude gen afterwards = \e f -> do gen e f; afterwards

----------------------------------------------------------------------
-- assign a specific register (for calling conventions)

assign :: Reg -> Arg -> Asm ()
assign = \case
  RegA -> loadA1
  RegX -> loadX
  RegY -> loadY
  ZP z -> store z

store :: ZeroPage -> Arg -> Asm ()
store target = \case
  Imm imm -> do trans (Ldai imm); trans (Sta target)
  Loc Located{a,x,y,z} ->
    if a then trans (Sta target) else do
      if x then trans (Stx target) else do
        if y then trans (Sty target) else do
          case z of
            -- TODO: optimmize if z and target are the same?
            Just z -> do trans (Ldaz z); trans (Sta target)
            Nothing -> undefined -- no example trigger this yet. Nope should do

----------------------------------------------------------------------
-- instruction selection

doublingA :: Gen
doublingA = \e ->  \case
  Op1 Asl arg -> do loadA arg; comp e Asla
  _ -> Nope

addition :: Gen
addition = \e -> \case
  Op2 Add arg1 arg2 ->
    if inAcc arg1 && inAcc arg2
    then comp e (Asla)
    else commutativeBinOp (addIntoA e) arg1 arg2
  _ ->
    Nope

addIntoA :: Exp -> Arg -> Asm ()
addIntoA e = \case
  Imm imm -> do clc; comp e (Adci imm)
  Loc Located{z} ->
    -- only location: Z (not A,X,Y)
    case z of
      Just z -> do clc; comp e (Adcz z)
      Nothing -> Nope


subtraction :: Gen
subtraction = \e -> \case
  Op2 Sub arg1 arg2 ->
    if inAcc arg1 then subIntoA e arg2 else
      do loadA arg1; subIntoA e arg2
  _ ->
    Nope

subIntoA :: Exp -> Arg -> Asm ()
subIntoA e = \case
  Imm imm -> do sec; comp e (Sbci imm)
  Loc Located{z} ->
    -- only location: Z (not A,X,Y)
    case z of
      Just z -> do sec; comp e (Sbcz z)
      Nothing -> Nope

xor :: Gen
xor = \e -> \case
  Op2 Xor arg1 arg2 ->
    commutativeBinOp (eorIntoA e) arg1 arg2
  _ ->
    Nope

eorIntoA :: Exp -> Arg -> Asm ()
eorIntoA e = \case
  Imm imm -> do comp e (Eori imm)
  Loc Located{z} ->
    -- only location: Z (not A,X,Y)
    case z of
      Just z -> do comp e (Eorz z)
      Nothing -> Nope


commutativeBinOp :: (Arg -> Asm ()) -> Arg -> Arg -> Asm ()
commutativeBinOp doOpInA arg1 arg2 = do
  if inAcc arg1 then doOpInA arg2 else
    if inAcc arg2 then doOpInA arg1 else
      if arg1 == arg2
      then do loadA arg1; doOpInA arg2
      else
        alternatives [ do loadA arg1; doOpInA arg2
                     , do loadA arg2; doOpInA arg1 ]


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
  Loc Located{z} ->
    case z of
      Just z -> pure z
      Nothing -> Nope

-- TODO: compile time constant folding

loadA :: Arg -> Asm ()
loadA arg = do
  loadA1 arg
  -- Load sharing...
  perhaps $ do trans Tax -- doing one is a bit expensive (improves example 99)
  -- perhaps $ do trans Tay -- too expensive at the moment (when testing all persm!)

loadA1 :: Arg -> Asm ()
loadA1 = \case
  Imm imm -> trans (Ldai imm)
  Loc Located{a,x,y,z} ->
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
  Loc Located{a,x,y,z} ->
    -- prefer location: X,A,Z (not Y)
    if x then pure () else do
      if a then trans Tax else
        case z of
          Just z -> trans (Ldxz z);
          Nothing ->
            if y then Nope else
              Nope

loadY :: Arg -> Asm ()
loadY = \case
  Imm imm -> trans (Ldyi imm)
  Loc Located{a,x,y,z} ->
    -- prefer location: Y,A,Z (not X)
    if y then pure () else do
      if a then trans Tay else
        case z of
          Just z -> trans (Ldyz z);
          Nothing ->
            if x then Nope else
              Nope

clc :: Asm ()
clc = Emit Clc noSemantics

sec :: Asm ()
sec = Emit Sec noSemantics

----------------------------------------------------------------------
-- spilling...

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

alternatives :: [Asm a] -> Asm a
alternatives = \case
  [] -> Nope
  x:xs -> foldl Alt x xs

----------------------------------------------------------------------
-- Located

locations :: Exp -> Asm Arg
locations exp = do
  xs <- Holding exp
  let _ = Print (printf "%s --> %s" (show exp) (show xs))
  let arg = Loc (classifyRegs xs)
  case xs of
    _:_ -> pure arg
    [] ->
      case exp of
        Exp (Num b) -> Alt (pure (Imm (Immediate b))) (pure arg)
        _ -> Nope

classifyRegs :: [Reg] -> Located
classifyRegs xs = do
  let nowhere = Located { a = False, x = False, y = False, z = Nothing }
  let
    f acc = \case
      RegA -> acc { a = True }
      RegX -> acc { x = True }
      RegY -> acc { y = True }
      ZP z -> acc { z = Just z }
  foldl f nowhere xs

everywhere :: Located -> [Reg]
everywhere Located{a,x,y,z} = do
  concat
    [ if a then [RegA] else []
    , if x then [RegX] else []
    , if y then [RegY] else []
    , case z of Just z -> [ZP z]; Nothing -> []
    ]

inAcc :: Arg -> Bool
inAcc = \case
  Imm{} -> False
  Loc Located{a} -> a
