-- |
-- Module:      VM
-- Author:      John L. Singleton <jls@cs.ucf.edu>
--
-- This module is responsible for implementing the CAM virtual machine.
-- 


module VM where

import Debug.Trace
import Text.Printf
import Data.List

data Value =
  ConstInt Int
  | ConstBool Bool
  | ConstString String
  | Pair (Ref Value) (Ref Value)
  | Adr [Instruction]
  | Nil
  deriving (Read, Show, Eq, Ord)


data Ref a = Ref Int ([a] -> a)

--
-- empty instance (we just need this to satisfy
-- the typechecker)
--

instance Read (Ref a) where
  

instance Ord (Ref a) where
  (Ref i1 _) `compare` (Ref i2 _) = i1 `compare` i2

instance Eq (Ref a) where
  (Ref i1 _) == (Ref i2 _) = i1 == i2

data Instruction =
  STOP
  | LOAD Value
  | PUSH Value
  | DUPL | SWAP | ROT3   | IROT3
  | FST  | SND  | SETFST | SETSND
  | CONS | SPLIT
  | ADD  | SUB  | MULT   | EQUAL
  | B_LT   | B_GT
  | CALL | RETURN
  | BRANCH Value Value
  deriving (Show, Read, Eq, Ord)


exec :: [Instruction]    -- the code
  -> [Value]             -- the memory
  -> [Value]             -- the stack
  -> Value               -- the resulting value of execution 
exec [ixn@STOP] m vs@[v]                         = _camTrace (ixn,m,vs) $ v
exec (ixn@(LOAD v):code) m@mem vs@(_:stack)      = _camTrace (ixn,m,vs) $ exec code mem (v:stack)
exec (ixn@(PUSH v):code) m@mem vs@stack          = _camTrace (ixn,m,vs) $ exec code mem (v:stack)
exec (ixn@DUPL:code) m@mem vs@(v:stack)          = _camTrace (ixn,m,vs) $ exec code mem (v:v:stack)
exec (ixn@SWAP:code) m@mem vs@(v:v':stack)       = _camTrace (ixn,m,vs) $ exec code mem (v':v:stack)
exec (ixn@ROT3:code) m@mem vs@(v1:v2:v3:stack)   = _camTrace (ixn,m,vs) $ exec code mem (v2:v3:v1:stack)
exec (ixn@IROT3:code) m@mem vs@(v1:v2:v3:stack)  = _camTrace (ixn,m,vs) $ exec code mem (v3:v1:v2:stack)
exec (ixn@FST:code) m@mem vs@((Pair (Ref _ v1) _):stack) = _camTrace (ixn,m,vs) $ exec code mem ((v1 mem):stack)
exec (ixn@SND:code) m@mem vs@((Pair _ (Ref _ v2)):stack) = _camTrace (ixn,m,vs) $ exec code mem ((v2 mem):stack)

exec (ixn@SETFST:code) m@mem vs@(v:(Pair (Ref i1 v1) (Ref i2 v2)):stack) = _camTrace (ixn, m, vs) $ let p = Pair (Ref i1 v1) (Ref i2 v2) in
  let mem' = replaceLocation i1 v mem in exec code mem' (p:stack)
exec (ixn@SETSND:code) m@mem vs@(v:(Pair (Ref i1 v1) (Ref i2 v2)):stack) = _camTrace (ixn, m, vs) $ let p = Pair (Ref i1 v1) (Ref i2 v2) in
  let mem' = replaceLocation i2 v mem in exec code mem' (p:stack)
exec (ixn@SPLIT:code) m@mem vs@((Pair (Ref _ v1) (Ref _ v2)):stack) = _camTrace (ixn, m, vs) $ exec code mem ((v1 mem):(v2 mem):stack)
exec (ixn@CONS:code) m@mem vs@(v1:v2:stack) = _camTrace (ixn, m, vs) $ let (p,mem') = makePair mem v1 v2 in exec code mem' (p:stack)


exec (ixn@ADD:code) m@mem vs@(ConstInt a:ConstInt b:stack) = _camTrace (ixn,m,vs) $ exec code mem ((ConstInt (a + b)):stack)
exec (ixn@SUB:code) m@mem vs@(ConstInt a:ConstInt b:stack) = _camTrace (ixn,m,vs) $ exec code mem ((ConstInt (a - b)):stack)
exec (ixn@MULT:code) m@mem vs@(ConstInt a:ConstInt b:stack) = _camTrace (ixn,m,vs) $ exec code mem ((ConstInt (a * b)):stack)
exec (ixn@EQUAL:code) m@mem vs@(a:b:stack) = _camTrace (ixn,m,vs) $ exec code mem ((ConstBool (a == b)):stack)
exec (ixn@B_LT:code) m@mem vs@(a:b:stack) = _camTrace (ixn,m,vs) $ exec code mem ((ConstBool (a < b)):stack)
exec (ixn@B_GT:code) m@mem vs@(a:b:stack) = _camTrace (ixn,m,vs) $ exec code mem ((ConstBool (a > b)):stack)


exec (ixn@CALL:code) m@mem vs@(Adr code':v:stack) = _camTrace (ixn,m,vs) $ exec code' mem (v:(Adr code:stack))
exec (ixn@RETURN:_) m@mem vs@(v:Adr code':stack) =  _camTrace (ixn,m,vs) $ exec code' mem (v:stack)
exec (ixn@(BRANCH adr1 adr2):code) m@mem vs@(ConstBool b:stack) = _camTrace (ixn,m,vs) $ if b then
                                                           exec code mem (adr1:stack)
                                                       else
                                                           exec code mem (adr2:stack)
-- fall thru case -- dump stack if this happens.
exec (a:_) _ stack = dumpStack stack $ error $ "Invalid Instruction: " ++ (show a)



formatBlock :: String -> [Value] -> String 
formatBlock title mem = printf "%s %s" title (tshowAr mem)

_camTrace (b@(BRANCH v1 v2),mem,values)= trace (printf "BRANCH\n\tIF   =%s\n\tELSE =%s\n%-30s %-10s %-10s" (tshow v1) (tshow v2) "BRANCH CONTROL INFO:" (formatBlock "MEM" mem) (formatBlock "STACK" values))

_camTrace (instruction,mem,values)= trace (printf "%-30s %-10s %-10s" (ishow instruction) (formatBlock "MEM" mem) (formatBlock "STACK" values))

dumpStack stack = trace (printf "[CAM FAULT]\n\n\nSTACKDUMP:\n\n %-10s" (formatBlock "STACK" stack))

instance Show (Ref a) where
  show (Ref idx _) = printf "#%s" (show idx)

-- this is super tricky; we model memory references as partially evaluated functions. 
makePair :: [Value] -> Value -> Value  -> (Value, [Value])
makePair mem v1 v2 = let mr = (length mem) in let p = Pair (Ref mr (access mr)) (Ref (mr+1) (access (mr+1))) in
  (p, mem ++ [v1,v2])
  where
    access n m = (m !! n)

replaceLocation :: Int -> Value -> [Value] -> [Value]
replaceLocation n v (x:xs)
     | n == 0 = (v:xs)
     | otherwise = x:replaceLocation (n-1) v xs  



tshow :: Value -> String
tshow (ConstInt a) = show a
tshow (ConstBool b) = show b
tshow (ConstString s) = s
tshow (Pair v1 _)     = printf "%s" (show v1)
tshow (Adr ixns) = printf "[%s]" (ishowAr ixns)
tshow Nil        = "Nil"


ishow :: Instruction -> String
ishow (LOAD a) = "LOAD " ++  (tshow a)
ishow (PUSH a) = "PUSH " ++  (tshow a)
ishow i = show i

tshowAr :: [Value] -> String 
tshowAr ar = "[" ++ (intercalate ":" $ map tshow ar) ++ "]"

ishowAr :: [Instruction] -> String 
ishowAr ar = "[" ++ (intercalate ":" $ map ishow ar) ++ "]"


