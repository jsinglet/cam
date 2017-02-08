module Lexer where
 

data Token
    = TokenBooleanLiteral Bool
    | TokenIntegerLiteral Int
    | TokenStringLiteral String
    | TokenEquals          -- i.e., == 
    | TokenLt
    | TokenGt
    | TokenAssign          -- i.e., = 
    | TokenAdd
    | TokenMinus
    | TokenMultiply
    | TokenDivide
    | TokenOpenParen
    | TokenCloseParen
    | TokenOpenPair
    | TokenClosePair
    | TokenIdent String
    | TokenLet
    | TokenLetRec
    | TokenIn
    | TokenIf
    | TokenThen
    | TokenElse
    | TokenFun
    | TokenArrow
    | TokenComma
    | TokenFst
    | TokenSnd
      deriving (Show, Eq)


data Special = Special Token String

singleSpecials :: [Special]
singleSpecials = [
    Special TokenLt         "<"
  , Special TokenGt         ">"
  , Special TokenAssign     "="
  , Special TokenAdd        "+"
  , Special TokenMinus      "-"
  , Special TokenOpenParen  "("
  , Special TokenCloseParen ")"
  , Special TokenMultiply   "*"
  , Special TokenDivide     "/"
  , Special TokenComma      ","
  , Special TokenOpenPair   "["
  , Special TokenClosePair  "]"
  ]

keywords :: [Special]
keywords = [
    Special TokenLet    "let"
  , Special TokenLetRec "rec"
  , Special TokenIn     "in"
  , Special TokenIf     "if"
  , Special TokenThen   "then"
  , Special TokenElse   "else"
  , Special TokenFun    "fun"
  , Special TokenSnd    "snd"
  , Special TokenFst    "fst"
  ]

compositeKeywords :: [([Token], Token)]
compositeKeywords = [
  ([TokenLet, TokenLetRec], TokenLetRec)]  -- "let rec"


convertCompositeKeywords :: [Token] -> [Token]
convertCompositeKeywords (x1:x2:xs) = let m = lookup [x1,x2] compositeKeywords in 
                                          case m of
                                            Just t -> t : (convertCompositeKeywords xs)
                                            _      -> x1 : (convertCompositeKeywords (x2:xs))
convertCompositeKeywords xs    = xs


isDigit :: Char -> Bool
isDigit c = c `elem` "0123456789"

isAlpha :: Char -> Bool
isAlpha c = c `elem` (['a'..'z'] ++ ['A'..'Z'] ++ "_")

singleSpecialToToken :: Char -> Token
singleSpecialToToken c = let Special t _:_ = filter (\(Special _ s) -> s == [c]) singleSpecials in t

isSpecial :: Char -> Bool
isSpecial c = [c] `elem` map (\(Special _ s) -> s) singleSpecials

keywordToToken :: String -> Token
keywordToToken c = let Special t _:_ = filter (\(Special _ s) -> s == c) keywords in t

isKeyword :: String -> Bool
isKeyword c = c `elem` map (\(Special _ s) -> s) keywords

isBoolean :: String -> Bool
isBoolean s = s == "True" || s == "False"

isEquals :: String -> Bool
isEquals s = s == "=="

isFun :: String -> Bool
isFun s = s == "fun"

isArrow :: String -> Bool
isArrow s = s == "->"

isIgnore :: Char -> Bool
isIgnore c =  c `elem` " \t\n\r"

-- always adding a padding space makes
-- the pattern matching equations just shorter and nicer
getTokens :: String -> [Token]
getTokens s = convertCompositeKeywords $ tokenize (s ++ " ")

tokenize :: String -> [Token]
tokenize [] = []
tokenize (x:xs)
  | isSpecial x = let t = singleSpecialToToken x in                     -- case 1, it's a special or an arrow
      -- we need to look ahead one if we have a minus, just in case we have an
      -- arrow operator
      if isSpecial (head xs) then let (t,rest) = readSpecial [x] xs in t : tokenize rest
      else t : tokenize xs 
  | isDigit x = let (t,rest) = readNumber [x] xs in  t : tokenize rest  -- case 2, it's a number
  | isIgnore x = tokenize xs                                            -- case 3, it's an ignore
  | isAlpha x  = let (t,rest) = readAlpha [x] xs in  t : tokenize rest  -- case 4, keywords, idents, etc
  | otherwise = error $ "\n\nTokenizer Failed on: " ++ show x

readSpecial :: String -> String -> (Token, String)
readSpecial acc s@(x:xs)
  -- we stop accumulating when we hit a non alpha character
  | not (isSpecial x) = accept (reverse acc)
  | otherwise  = readSpecial (x:acc) xs
  where
    accept a
      | isEquals a  = (TokenEquals, s)
      | isArrow a  = (TokenArrow, s)
      | otherwise  = error $ "Unexpected Token: " ++ show a

eatIgnores (x:xs)
  | isIgnore x = eatIgnores xs
  | otherwise  = x:xs


nextNonEmptyTokenIs :: Token -> String -> Bool
nextNonEmptyTokenIs t xs = t == head (tokenize xs)

readAlpha :: String -> String -> (Token, String)
readAlpha acc s@(x:xs)
  -- we stop accumulating when we hit a non alpha character
  | not (isAlpha x) = accept (reverse acc)
  | otherwise  = readAlpha (x:acc) xs
  where
    accept a
      | isKeyword a  = (keywordToToken a, s)
      | isBoolean a  = (TokenBooleanLiteral (read a :: Bool), s)
      | isEquals  a  = (TokenEquals, s)
      | otherwise      = (TokenIdent a, s)

readNumber :: String -> String -> (Token,String)
readNumber acc s@(x:xs)
  | not (isDigit x) = (TokenIntegerLiteral (read (reverse acc) :: Int), s)
  | isDigit  x = readNumber (x:acc) xs
  | otherwise  = error $ "Invalid number: " ++ acc
