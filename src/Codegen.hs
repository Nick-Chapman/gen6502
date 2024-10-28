module Codegen
  ( perhaps, preamble
  , assign, Reg, Name, Arg(..)
  , codegenBranch
  , spillA, spillX, spillY
  , codegen1, codegenPred1 --old
  , codegenNew, codegenPredNew --new
  , spillAnyContents
  , Need, needNothing, needName, needUnion
  ) where

import Text.Printf (printf)
import Prelude hiding (exp,compare,and)
import Asm (AsmState(..),Asm(..))
import Instruction (Instruction(..),ITransfer(..),ICompute(..),ICompare(..),transferSemantics,computeSemantics,compareSemantics)
import Semantics (SemState,Semantics,Reg(..),ZeroPage(..),Immediate(..),noSemantics,Name,Arg(..),makeSem,Oper(..),getFreshName,findSemOper,findSemState)

import Semantics (Arg1(..),Pred(..),makeSem1,Flag(..),lookupReg)
--import Instruction

import Data.Set (Set,member)
import qualified Data.Set as Set


data Need = Need { names :: Set Name }

instance Show Need where
  show Need{names} = show (Set.toList names)

needNothing :: Need
needNothing = Need { names = Set.empty }

isNeeded :: Name -> Need -> Bool
isNeeded name Need{names} = name `member` names

needName :: Name -> Need
needName name = Need { names = Set.singleton name }

needUnion :: Need -> Need -> Need
needUnion Need{names=ns1} Need{names=ns2} = Need {names = ns1 `Set.union` ns2 }

----------------------------------------------------------------------
-- lazy spill only when necessary

codegenNew :: Need -> Oper -> Asm Arg
codegenNew need oper =
  alternatives
  [ do needSpill need RegA; driveA (Down oper) oper
  , do needSpill need RegX; driveX (Down oper) oper
  , do needSpill need RegY; driveY (Down oper) oper
  , driveZ (Down oper) oper
  , do numeric (Down oper) oper
  ]

codegenPredNew :: Need -> Pred -> Asm Arg1
codegenPredNew need pred = do
  needSpill need RegA
  codegenPred1 pred

needSpill :: Need -> Reg -> Asm ()
needSpill need reg = do
  whatsIn reg >>= \case
    Nothing -> do
      --Io (printf "needSpill(%s): No need; it's empty\n" (show reg))
      pure ()
    Just name -> do
      let b = isNeeded name need
      if not b then pure () else do
        let _ = Io (printf "needSpill--needed(%s): %s MEM %s\n" (show reg) (show name) (show need))
        spill reg

  pure ()


spillAnyContents :: Reg -> Asm ()
spillAnyContents reg =
  whatsIn reg >>= \case
    Nothing -> pure ()
    Just _ -> spill reg

whatsIn :: Reg -> Asm (Maybe Name)
whatsIn reg = do
  ss <- querySS
  pure (lookupReg ss reg)

spill :: Reg -> Asm ()
spill = \case
  RegA -> spillA
  RegX -> spillX
  RegY -> spillY
  ZP{} -> error "spill-ZP"

----------------------------------------------------------------------
-- predicates

-- TODO: check if alreay computed
codegenPred1 :: Pred -> Asm Arg1
codegenPred1 p =
  -- TODO: select for diff predictes here
  cmp p p


cmp :: Pred -> Pred -> Asm Arg1
cmp p = \case
  Equal arg1 arg2 -> do
    --do loadA arg1; cmpIntoA p arg2 -- TODO: try either way! -okay...
    alternatives [ do loadA arg1; cmpIntoA p arg2 , do loadA arg2; cmpIntoA p arg1 ]
--  _ ->
--    Nope

cmpIntoA :: Pred -> Arg -> Asm Arg1
cmpIntoA p = \case
  Imm imm -> do compare p (Cmpi imm)
  Name name -> do
    Located{z} <- locations name
    -- only location: Z (not A,X,Y)
    case z of
      Just z -> do compare p (Cmpz z)
      Nothing -> Nope


codegenBranch :: Arg1 -> Asm Flag
codegenBranch _ =
  -- This needs to find out which flag (if any) is holding the compute Arg1 predicate
  -- But current we have only one flag
  -- and we are not tracking it yet!
  -- so lets just return it, and have emulation show when this is wrong
  --undefined
  pure FlagZ


----------------------------------------------------------------------
-- instruction selection and code generation

preamble :: Asm ()
preamble = do
  perhaps spillA
  perhaps spillX
  perhaps spillY

codegen1 :: Oper -> Asm Arg
codegen1 oper = do
  ss <- querySS
  let xm = findSemOper ss oper
  case xm of
    Just name -> pure (Name name)
    Nothing -> codegen1g (Down oper) oper

data Down = Down (Oper)

type Gen = Down -> Oper -> Asm Arg

codegen1g :: Gen
codegen1g = select
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
driveA = select [doublingA,halvingA,addition,subtraction,and,eor]
driveX = select [incrementX]
driveY = select [incrementY]
driveZ = select [incrementZ,doublingZ,halvingZ]

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
  Asl arg -> do loadA arg; compute e Asla
  _ -> Nope

halvingA :: Gen
halvingA = \e -> \case
  Lsr arg -> do loadA arg; compute e Lsra
  _ -> Nope

addition :: Gen
addition = \e -> \case
  Add arg1 arg2 -> do
    b1 <- inAcc arg1
    b2 <- inAcc arg2
    if b1 && b2
    then compute e (Asla)
    else commutativeBinOp (addIntoA e) arg1 arg2
  _ ->
    Nope

addIntoA :: Down -> Arg -> Asm Arg
addIntoA e = \case
  Imm imm -> do clc; compute e (Adci imm)
  Name name -> do
    Located{z} <- locations name
    -- only location: Z (not A,X,Y)
    case z of
      Just z -> do clc; compute e (Adcz z)
      Nothing -> Nope

subtraction :: Gen
subtraction = \e -> \case
  Sub arg1 arg2 -> do
    do loadA arg1; subIntoA e arg2
  _ ->
    Nope

subIntoA :: Down -> Arg -> Asm Arg
subIntoA e = \case
  Imm imm -> do sec; compute e (Sbci imm)
  Name name -> do
    Located{z} <- locations name
    -- only location: Z (not A,X,Y)
    case z of
      Just z -> do sec; compute e (Sbcz z)
      Nothing -> Nope

and :: Gen
and = \e -> \case
  And arg1 arg2 ->
    commutativeBinOp (andIntoA e) arg1 arg2
  _ ->
    Nope

andIntoA :: Down -> Arg -> Asm Arg
andIntoA e = \case
  Imm imm -> do compute e (Andi imm)
  Name name -> do
    Located{z} <- locations name
    -- only location: Z (not A,X,Y)
    case z of
      Just z -> do compute e (Andz z)
      Nothing -> Nope

eor :: Gen
eor = \e -> \case
  Eor arg1 arg2 ->
    commutativeBinOp (eorIntoA e) arg1 arg2
  _ ->
    Nope

eorIntoA :: Down -> Arg -> Asm Arg
eorIntoA e = \case
  Imm imm -> do compute e (Eori imm)
  Name name -> do
    Located{z} <- locations name
    -- only location: Z (not A,X,Y)
    case z of
      Just z -> do compute e (Eorz z)
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
          alternatives [ do loadA arg1; doOpInA arg2 , do loadA arg2; doOpInA arg1 ] -- TEMP elide
          --do loadA arg1; doOpInA arg2


incrementX :: Gen
incrementX = \e -> \case
  Add arg (Imm 1) -> do loadX arg; compute e Inx
  Add (Imm 1) arg -> do loadX arg; compute e Inx
  _ -> Nope

incrementY :: Gen
incrementY = \e -> \case
  Add arg (Imm 1) -> do loadY arg; compute e Iny
  Add (Imm 1) arg -> do loadY arg; compute e Iny
  _ -> Nope

incrementZ :: Gen
incrementZ e = \case
  Add arg (Imm 1) -> do z <- inZP arg; compute e (Incz z)
  Add (Imm 1) arg -> do z <- inZP arg; compute e (Incz z)
  _ -> Nope

doublingZ :: Gen
doublingZ = \e ->  \case
  Asl arg -> do z <- inZP arg; compute e (Aslz z)
  _ -> Nope

halvingZ :: Gen
halvingZ = \e ->  \case
  Lsr arg -> do z <- inZP arg; compute e (Lsrz z)
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

  -- TEMP elide load sharing
  -- perhaps $ do trans Tax -- doing one is a bit expensive (improves example 99)
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
clc = emitWithSemantics Clc noSemantics

sec :: Asm ()
sec = emitWithSemantics Sec noSemantics

----------------------------------------------------------------------
-- spilling...

spillA :: Asm ()
spillA = do
  z <- freshTemp
  --Io (printf "spillA -> %s\n" (show z))
  trans (Sta z)

spillX :: Asm ()
spillX = do
  z <- freshTemp
  trans (Stx z)

spillY :: Asm ()
spillY = do
  z <- freshTemp
  trans (Sty z)

trans :: ITransfer -> Asm ()
trans i = emitWithSemantics (Tx i) (transferSemantics i)

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

compute :: Down -> ICompute -> Asm Arg
compute (Down form) i = do
  name <- freshName
  let sem = makeSem name form
  --Io (printf "compute: %s = %s\n" (show name) (show form))
  emitWithSemantics (Compute i) (computeSemantics sem i)
  pure (Name name)

compare :: Pred -> ICompare -> Asm Arg1
compare p i = do
  name <- freshName
  let sem1 = makeSem1 name p
  --Io (printf "compare: %s = %s\n" (show name) (show p))
  emitWithSemantics (Compare i) (compareSemantics sem1 i)
  pure (Name1 name)

emitWithSemantics :: Instruction -> Semantics -> Asm ()
emitWithSemantics i semantics = do
  Emit i
  updateSS $ \ss -> ((), semantics ss)

freshName :: Asm Name
freshName = updateSS getFreshName

updateSS :: (SemState -> (a,SemState)) -> Asm a
updateSS m =
  Update (\s -> do
             let AsmState {ss} = s
             let (a,ss') = m ss
             let s' = s { ss = ss' }
             (a,s'))

querySS :: Asm SemState
querySS =
  Update (\s -> do
             let AsmState {ss} = s
             (ss,s))

freshTemp :: Asm ZeroPage
freshTemp =
  Update (\s -> do
             let AsmState {temps} = s
             case temps of
               [] -> error "run out of temps"
               (firstTemp:temps) -> do
                 let s' = s { temps }
                 (firstTemp, s'))

----------------------------------------------------------------------
-- Located

locations :: Name -> Asm Located
locations name = do
  ss <- querySS
  let xs = findSemState ss name
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
