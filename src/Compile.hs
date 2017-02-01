module Compile where

import Lang
import VM

compile_unop :: M_UNOP -> Instruction 
compile_unop M_FST = FST
compile_unop M_SND = SND

compile_binop :: M_BINOP -> Instruction
compile_binop M_ADD = ADD
compile_binop M_SUB = SUB
compile_binop M_MULT = MULT
compile_binop M_EQ = EQUAL
compile_binop M_LT = B_LT
compile_binop M_GT = B_GT

compile_var :: (Show a, Eq a) =>  [a] -> a -> [Instruction]
compile_var env v = case env of
  [] -> error $ "Compile Error, Unbound Variable" ++ (show v)
  (x:envs) -> if x == v then [FST] else SND:(compile_var envs v)

init_stack :: [Value]
init_stack = [Nil]

eval :: EXP -> Value
eval e = exec (compile e) [] init_stack

evalInstructions :: [Instruction] -> Value
evalInstructions i = exec i [] init_stack


compile :: EXP -> [Instruction]
compile e = (comp [] e) ++ [STOP]

-- constants 
comp env (EXP_INT n)  = [LOAD (ConstInt n) ]
comp env (EXP_BOOL b) = [LOAD (ConstBool b)]
comp env (EXP_STRING s) = [LOAD (ConstString s)]

-- pairs and ops
comp env (EXP_PAIR e1 e2) = [DUPL] ++ (comp env e2) ++ [SWAP] ++ (comp env e1) ++ [CONS]

comp env (EXP_UNOP op e) = (comp env e)++[compile_unop op]

comp env (EXP_BINOP op e1 e2) = [DUPL] ++ (comp env e2) ++ [SWAP] ++ (comp env e1) ++ [compile_binop op]

-- variables
comp env (EXP_VAR v) = compile_var env v

-- control structures 
comp env (EXP_IF e1 e2 e3) = [DUPL] ++ (comp env e1)
  ++ [BRANCH (Adr(comp env e2 ++ [RETURN])) (Adr(comp env e3 ++ [RETURN])), CALL]

-- functions and application 
comp env (EXP_FUN x e) = [PUSH (Adr(comp (x:env) e ++ [RETURN])), SWAP, CONS]

comp env (EXP_APP e1 e2) = [DUPL] ++ (comp env e2) ++ [SWAP] ++ (comp env e1)
  ++ [SPLIT, IROT3, CONS, SWAP, CALL]

-- lets
comp env (EXP_LET x e1 e2) = [DUPL] ++ (comp env e1) ++ [CONS] ++ (comp (x:env) e2)

comp env (EXP_LETREC f e1@(EXP_FUN _ _) e2) = [PUSH Nil] ++ (comp (f:env) e1)
                                              ++ [DUPL, ROT3, CONS, SETFST, FST]
                                              ++ (comp (f:env) e2)

comp env (EXP_LETREC f e1 e2) = [DUPL, PUSH Nil, DUPL, CONS, DUPL, ROT3, CONS]
                                ++ (comp (f:env) e1)
                                ++ [DUPL, ROT3, FST, SETFST, SWAP, SND, SETSND, CONS]
                                ++ (comp (f:env) e2)




example1c = compile $ EXP_BINOP M_ADD (EXP_INT 1) (EXP_INT 1)
example1  = EXP_BINOP M_ADD (EXP_INT 1) (EXP_INT 1)

