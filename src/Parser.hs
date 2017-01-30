module Parser where

import Lexer
import Lang
import Debug.Trace
import Compile


{- 
Abstract Syntax
===============

e ::=
  | ident
  | e1 e2      -- application
  | e1 op e2   -- binop 
  | (e1, e2)   -- pair
  | if e1 then e2 else e3
  | let {rec} p = e in e
  | fun p -> e

p ::=
  | ()
  | (p1, p2) 
-}

test1 = expression $ getTokens "1 + 1"
test2 = expression $ getTokens "1 + let a = 3 in a"
test3 = expression $ getTokens "if 1 == 1 then 2 else 3"
test4 = expression $ getTokens "1 + if 1 == 2 then 2 else 3"
test5 = expression $ getTokens "fun x -> x + 3 3"

test1c = compile $ fst test1
test2c = compile $ fst test2
test3c = compile $ fst test3
test4c = compile $ fst test4

test1e = eval $ fst test1
test2e = eval $ fst test2 
test3e = eval $ fst test3
test4e = eval $ fst test4


terms :: [Token]
terms =  [TokenAdd, TokenMinus, TokenEquals]

factors :: [Token]
factors =  [TokenMultiply, TokenDivide, TokenGt, TokenLt]

ops = factors ++ terms

getOp :: Token -> M_BINOP
getOp t = case t of
  (TokenAdd) -> M_ADD
  (TokenMinus) -> M_SUB
  (TokenMultiply) -> M_MULT
  (TokenEquals)   -> M_EQ
  (TokenLt)       -> M_LT
  (TokenGt)       -> M_GT
  (TokenDivide)   -> M_DIV
  _               -> error $ "Parser Error: Unknown Operator " ++ (show t)

term :: [Token] -> (EXP,[Token])
term (TokenBooleanLiteral b:xs) = (EXP_BOOL b, xs)
term (TokenIntegerLiteral i:xs) = (EXP_INT  i, xs)
term (TokenIdent s:xs) = (EXP_VAR s, xs)
term t = error $ "Unknown Term: " ++ (show t)

expression :: [Token] -> (EXP, [Token])

-- pairs
expression (TokenOpenParen:xs) = 
  let (p1, rest) = expression xs in
    let rest1 = skip rest TokenComma in
      let (p2, rest2) = expression rest1 in
        let rest3 = skip rest2 TokenCloseParen in 
          (EXP_PAIR p1 p2, rest3)

-- structures
expression (TokenFst:xs) = 
  let (e1, rest) = expression xs in (EXP_UNOP M_FST e1, rest)
expression (TokenSnd:xs) = 
  let (e1, rest) = expression xs in (EXP_UNOP M_SND e1, rest)

expression (TokenIf:xs) = 
  let (e1, rest) = expression xs in
    let (e2, rest1) = expression (skip rest TokenThen) in
      let (e3, rest2) = expression (skip rest1 TokenElse) in (EXP_IF e1 e2 e3, rest2)

-- lambdas
expression (TokenFun:xs) = case (xs) of
  (TokenIdent i:xs) -> let rest = (skip xs TokenArrow) in
    let (e1, rest1) = expression rest in
      maybeApply (EXP_FUN i e1, rest1)
  _                 -> error $ "Expected a functional parameter (ident) following lambda but found: " ++ (show (head xs))

  
-- application


expression (TokenLet:xs) = case (xs) of
  (TokenIdent i:xs) -> let rest = (skip xs TokenAssign) in
    let (e1, rest1) = expression rest in
      let rest2 = skip rest1 TokenIn in
        let (e2, rest3) = expression rest2 in
          (EXP_LET i e1 e2, rest3)
  _      -> error $ "Parse Error: Expected Ident following let but found: " ++ (show $ head xs)

expression (TokenLetRec:xs) = case (xs) of
  (TokenIdent i:xs) -> let rest = (skip xs TokenAssign) in
    let (e1, rest1) = expression rest in
      let rest2 = skip rest1 TokenIn in
        let (e2, rest3) = expression rest2 in
          (EXP_LETREC i e1 e2, rest3)
  _      -> error $ "Expression Error: Expected Ident following letrec but found: " ++ (show $ head xs)


expression xs = let (e1, rest) = term xs in
  if rest == [] then (e1, rest) else
  if (head rest) `elem` ops then 
  let (op:rest1) = rest in
    let operator = getOp op in
      let (e2, rest2) = expression rest1 in
        (EXP_BINOP operator e1 e2, rest2)
  else
    (e1, rest)


maybeApply :: (EXP, [Token]) -> (EXP, [Token])
maybeApply (e, t@(x:xs)) = let (e',tokens) = expression t in (EXP_APP e e', tokens)
maybeApply (e, [])     = (e, [])


skip :: [Token] -> Token -> [Token]
skip (x:rest) t = if t ==x then rest else error $ "Parse Error: Expected " ++ (show t)
                                          ++ " but found " ++ (show x)

