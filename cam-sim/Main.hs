module Main where

import Lexer


main :: IO ()
main = putStrLn $  show $ getTokens "hello world"
