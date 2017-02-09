{-# LANGUAGE PatternGuards #-}

module Lang where

-- mini-ml language definition


data M_UNOP = M_FST | M_SND
  deriving (Show, Eq)
data M_BINOP = M_ADD | M_SUB | M_MULT | M_EQ | M_LT | M_GT | M_DIV
  deriving (Show, Eq)

data EXP = EXP_INT Int | EXP_BOOL Bool | EXP_STRING String | EXP_PAIR EXP EXP
  | EXP_UNOP M_UNOP EXP
  | EXP_BINOP M_BINOP EXP EXP
  | EXP_VAR String
  | EXP_IF EXP EXP EXP
  | EXP_FUN String EXP
  | EXP_APP EXP EXP
  | EXP_LET String EXP EXP
  | EXP_LETREC String EXP EXP
  deriving (Show, Eq)
