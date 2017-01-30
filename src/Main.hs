{-# LANGUAGE QuasiQuotes #-} 

-- |
-- Module:      Main
-- Author:      John L. Singleton <jls@cs.ucf.edu>

module Main where

import System.Environment  
import QQ
import Lexer
import Compile
import VM
import Parser
import System.FilePath

main :: IO ()
main = do  
    command <- getArgs
    case command of
      (h:t) -> do
         let action = lookup h dispatch

         case action of
           Just a -> a (head t)
           Nothing -> invalidUsage
      _     -> invalidUsage

helpMessage :: String
helpMessage = [qq|Usage: cam [ -lex | -parse | -vm | -run | -compile ] FILE
-lex:    	Lex the source of the program contained in FILE.
-parse:		Parse the source of program contained in FILE (implies -lex).
-vm:		Execute the VM code contained in FILE.
-run:           Lex, Parse, and Execute the code contained in FILE.
-compile:       Compile FILE to byte code. Outputs FILE.o. 
|]


dispatch :: [(String, (String -> IO ()))]  
dispatch =  [   ("-vm", doVM)
              , ("-lex", doLex)
              , ("-parse", doParse)
              , ("-run",   doRun)
              , ("-compile", doCompile)
            ]

invalidUsage :: IO ()
invalidUsage = do
  putStrLn "Invalid Arguments"
  help


help :: IO ()
help = putStrLn helpMessage

-- | Main Operations

doVM :: String -> IO ()
doVM f = 
  do 
    file <- readFile f
    let instructions = read file :: [Instruction]
    putStrLn "Starting VM"
    putStrLn $ "\n\nParsing Instructions: " ++ (show file)
    putStrLn $ "\n\nLoaded Instructions: " ++ (show instructions) ++ "\n\n"
    let x = evalInstructions instructions
    putStrLn $ show x
    putStrLn "DONE."


doLex :: String -> IO ()
doLex f =
  do
    file <- readFile f
    putStrLn $ "Program: "
    putStrLn   "========="
    putStrLn (show file)
    putStrLn "\n\nLexer Output:"
    putStrLn "============="
    putStrLn $ show (getTokens file)
    

doParse :: String -> IO ()
doParse f =
  do
    file <- readFile f
    doLex f
    putStrLn "\n\nParser Output:"
    putStrLn "=================="
    putStrLn $ show $ expression (getTokens file)
    putStrLn "\n\nGenerated Instructions: "
    putStrLn "============================"
    putStrLn $ show $ compile $ fst $ expression (getTokens file)

doRun :: String -> IO ()
doRun f =
  do
    file <- readFile f
    doParse f
    let e = eval $ fst $ expression (getTokens file)
    putStrLn (show e)


doCompile :: String -> IO ()
doCompile f =
  do
    file <- readFile f
    let e = compile $ fst $ expression (getTokens file)
    writeFile (replaceExtension f ".o") (show e)
    putStrLn $ "File written to: " ++ (replaceExtension f ".o")
    

