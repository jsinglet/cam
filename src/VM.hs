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
           deriving (Read, Show)


data Ref a = Ref Int a
  deriving (Read)

data Instruction =
  STOP
  | LOAD Value
  | PUSH Value
  | DUPL | SWAP | ROT3   | IROT3
  | FST  | SND  | SETFST | SETSND
  | CONS | SPLIT
  | ADD  | SUB  | MULT   | EQUAL
  | CALL | RETURN
  | BRANCH Value Value
  deriving (Show, Read)

formatBlock :: String -> [Value] -> String 
formatBlock title mem = printf "%s %s" title (tshowAr mem)

_camTrace (b@(BRANCH v1 v2),mem,values)= trace (printf "BRANCH\n\tIF   =%s\n\tELSE =%s\n%-30s %-10s %-10s" (tshow v1) (tshow v2) "BRANCH CONTROL INFO:" (formatBlock "MEM" mem) (formatBlock "STACK" values))

_camTrace (instruction,mem,values)= trace (printf "%-30s %-10s %-10s" (ishow instruction) (formatBlock "MEM" mem) (formatBlock "STACK" values))

instance Show (Ref a) where
  show (Ref idx b) = printf "#%s" (show idx)

ref :: [Value] -> Ref a -> Value
ref mem b = ConstInt 3

makePair :: [Value] -> Value -> Value  -> (Value, [Value])
makePair mem v1 v2 = let mr = (length mem) in let p = Pair (Ref mr v1) (Ref (mr+1) v2) in
  (p, mem ++ [v1,v2])

replaceLocation :: Int -> a -> [a] -> [a]
replaceLocation n v (x:xs)
     | n == 0 = (v:xs)
     | otherwise = x:replaceLocation (n-1) v xs  

exec :: [Instruction]    -- the code
  -> [Value]             -- the memory
  -> [Value]             -- the stack
  -> Value               -- the resulting value of execution 
exec [ixn@STOP] m@mem vs@[v]                     = _camTrace (ixn,m,vs) $ v
exec (ixn@(LOAD v):code) m@mem vs@(_:stack)      = _camTrace (ixn,m,vs) $ exec code mem (v:stack)
exec (ixn@(PUSH v):code) m@mem vs@stack          = _camTrace (ixn,m,vs) $ exec code mem (v:stack)
exec (ixn@DUPL:code) m@mem vs@(v:stack)          = _camTrace (ixn,m,vs) $ exec code mem (v:v:stack)
exec (ixn@SWAP:code) m@mem vs@(v:v':stack)       = _camTrace (ixn,m,vs) $ exec code mem (v':v:stack)
exec (ixn@ROT3:code) m@mem vs@(v1:v2:v3:stack)   = _camTrace (ixn,m,vs) $ exec code mem (v2:v3:v1:stack)
exec (ixn@IROT3:code) m@mem vs@(v1:v2:v3:stack)  = _camTrace (ixn,m,vs) $ exec code mem (v3:v1:v2:stack)
exec (ixn@FST:code) m@mem vs@((Pair (Ref _ v1) _):stack) = _camTrace (ixn,m,vs) $ exec code mem (v1:stack)
exec (ixn@SND:code) m@mem vs@((Pair _ (Ref _ v2)):stack) = _camTrace (ixn,m,vs) $ exec code mem (v2:stack)


exec (ixn@SETFST:code) m@mem vs@(v:(Pair (Ref i1 v1) (Ref i2 v2)):stack) = _camTrace (ixn, m, vs) $ let p = Pair (Ref i1 v) (Ref i2 v2) in
  let mem' = replaceLocation i1 v mem in exec code mem' (p:stack)

exec (ixn@SETSND:code) m@mem vs@(v:(Pair (Ref i1 v1) (Ref i2 v2)):stack) = _camTrace (ixn, m, vs) $ let p = Pair (Ref i1 v1) (Ref i2 v) in
  let mem' = replaceLocation i2 v mem in exec code mem' (p:stack)


exec (ixn@SPLIT:code) m@mem vs@((Pair (Ref _ v1) (Ref _ v2)):stack) = _camTrace (ixn, m, vs) $ exec code mem (v1:v2:stack)
exec (ixn@CONS:code) m@mem vs@(v1:v2:stack) = _camTrace (ixn, m, vs) $ let (p,mem') = makePair mem v1 v2 in exec code mem' (p:stack)
exec (ixn@ADD:code) m@mem vs@(ConstInt a:ConstInt b:stack) = _camTrace (ixn,m,vs) $ exec code mem ((ConstInt (a + b)):stack)
exec (ixn@SUB:code) m@mem vs@(ConstInt a:ConstInt b:stack) = _camTrace (ixn,m,vs) $ exec code mem ((ConstInt (a - b)):stack)
exec (ixn@MULT:code) m@mem vs@(ConstInt a:ConstInt b:stack) = _camTrace (ixn,m,vs) $ exec code mem ((ConstInt (a * b)):stack)
exec (ixn@EQUAL:code) m@mem vs@(ConstInt a:ConstInt b:stack) = _camTrace (ixn,m,vs) $ exec code mem ((ConstBool (a == b)):stack)
exec (ixn@CALL:code) m@mem vs@(Adr code':v:stack) = _camTrace (ixn,m,vs) $ exec code' mem (v:(Adr code:stack))
exec (ixn@RETURN:_) m@mem vs@(v:Adr code':stack) =  _camTrace (ixn,m,vs) $ exec code' mem (v:stack)
exec (ixn@(BRANCH adr1 adr2):code) m@mem vs@(ConstBool b:stack) = _camTrace (ixn,m,vs) $ if b then
                                                           exec code mem (adr1:stack)
                                                       else
                                                           exec code mem (adr2:stack)
-- fall thru case
exec (a:_) _ _ = error $ "Invalid Instruction: " ++ (show a)







-- dereference memory location 
(#!) = 0


tshow :: Value -> String
tshow (ConstInt a) = show a
tshow (ConstBool b) = show b
tshow (ConstString s) = s
tshow (Pair v1 _)     = printf "%s" (show v1)
--show (Pair v1 v2) = printf "(%s, %s)" (show v1) (show v2)
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


