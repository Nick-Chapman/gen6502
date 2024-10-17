module Codegen
  ( preamble, codegen, assign, Reg, Name, Arg(..)
  ) where

import Asm (Asm(..))
import Instruction (Instruction(..),ITransfer(..),ICompute(..),transferSemantics,computeSemantics)
import Language (Form(..),Op2(..),Op1(..))
import Semantics (Reg(..),ZeroPage(..),Immediate(..),noSemantics,Name,Arg(..),makeSem)

----------------------------------------------------------------------
-- instruction selection and code generation

preamble :: Asm ()
preamble = do
  perhaps spillA
  perhaps spillX
  perhaps spillY

type GenX = Form Arg -> Asm Arg

codegen :: GenX
codegen f = do
  let oper = f
  xm <- FindOper oper
  case xm of
    Just name -> pure (Name name)
    Nothing -> codegen1 (Down f) f

data Down = Down (Form Arg)

type Gen = Down -> Form Arg -> Asm Arg

codegen1 :: Gen
codegen1 = select
  [ do driveA `conclude` perhaps spillA
  , do driveX `conclude` perhaps spillX
  , do driveY `conclude` perhaps spillY
  , do driveZ
  , do numeric
  ]

numeric :: Gen
numeric _e = \case
  Num n -> pure (Imm (Immediate n))
  _ -> Nope

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
conclude gen afterwards = \e f -> do
  arg <- gen e f
  afterwards
  pure arg

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
  Name name -> do
    Located{a,x,y,z} <- locations name
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
doublingA = \e -> \case
  Op1 Asl arg -> do loadA arg; comp e Asla
  _ -> Nope

addition :: Gen
addition = \e -> \case
  Op2 Add arg1 arg2 -> do
    b1 <- inAcc arg1
    b2 <- inAcc arg2
    if b1 && b2
    then comp e (Asla)
    else commutativeBinOp (addIntoA e) arg1 arg2
  _ ->
    Nope

addIntoA :: Down -> Arg -> Asm Arg
addIntoA e = \case
  Imm imm -> do clc; comp e (Adci imm)
  Name name -> do
    Located{z} <- locations name
    -- only location: Z (not A,X,Y)
    case z of
      Just z -> do clc; comp e (Adcz z)
      Nothing -> Nope

subtraction :: Gen
subtraction = \e -> \case
  Op2 Sub arg1 arg2 -> do
    do loadA arg1; subIntoA e arg2
  _ ->
    Nope

subIntoA :: Down -> Arg -> Asm Arg
subIntoA e = \case
  Imm imm -> do sec; comp e (Sbci imm)
  Name name -> do
    Located{z} <- locations name
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

eorIntoA :: Down -> Arg -> Asm Arg
eorIntoA e = \case
  Imm imm -> do comp e (Eori imm)
  Name name -> do
    Located{z} <- locations name
    -- only location: Z (not A,X,Y)
    case z of
      Just z -> do comp e (Eorz z)
      Nothing -> Nope


commutativeBinOp :: (Arg -> Asm Arg) -> Arg -> Arg -> Asm Arg
commutativeBinOp doOpInA arg1 arg2 = do
  inAcc arg1 >>= \case
    True -> doOpInA arg2
    False ->
      inAcc arg2 >>= \case
      True -> doOpInA arg1
      False ->
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
  Name name -> do
    Located{z} <- locations name
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
  Name name -> do
    Located{a,x,y,z} <- locations name
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
  Name name -> do
    Located{a,x,y,z} <- locations name
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
  Name name -> do
    Located{a,x,y,z} <- locations name
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
  z <- FreshTemp
  trans (Sta z)

spillX :: Asm ()
spillX = do
  z <- FreshTemp
  trans (Stx z)

spillY :: Asm ()
spillY = do
  z <- FreshTemp
  trans (Sty z)

trans :: ITransfer -> Asm ()
trans i = Emit (Tx i) (transferSemantics i)

alternatives :: [Asm a] -> Asm a
alternatives = \case
  [] -> Nope
  x:xs -> foldl Alt x xs

inAcc :: Arg -> Asm Bool
inAcc = \case
  Imm{} -> pure False
  Name name -> do
    Located{a} <- locations name
    pure a

----------------------------------------------------------------------
-- generate code with computation effect

comp :: Down -> ICompute -> Asm Arg
comp (Down form) i = do
  name <- FreshName
  let sem = makeSem name form
  Emit (Comp i) (computeSemantics sem i)
  pure (Name name)

----------------------------------------------------------------------
-- Located

locations :: Name -> Asm Located
locations name = do
  xs <- FindName name
  pure (classifyRegs xs)

data Located = Located { a,x,y::Bool,z::Maybe ZeroPage } deriving (Eq)

instance Show Located where
  show located = show (everywhere located)

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
