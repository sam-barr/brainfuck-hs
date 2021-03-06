{-# LANGUAGE TupleSections #-}
module Brainfuck (runbf) where

import Data.Char
import Data.Bifunctor
import System.IO
import System.Environment
import Tape

-- Data type for brainfuck character
-- treats expressions in between brackets as a single character
data BFChar = RIGHT
            | LEFT
            | INC
            | DEC
            | PRINT
            | READ
            | BRACKET BFExp
     deriving (Eq)

-- a brainfuck expression is just a list of brainfuck characters
type BFExp = [BFChar]

-- code parser
parse :: String -> (BFExp, String)
parse []     = ([],[])
parse (c:cs) = 
  case c of '>' -> app RIGHT
            '<' -> app LEFT
            '+' -> app INC
            '-' -> app DEC
            '.' -> app PRINT
            ',' -> app READ
            '[' -> if null recRest then 
                     error "Unexpected EOF when parsing" 
                   else 
                     first (BRACKET recExp:) $ parse $ tail recRest
            ']' -> ([],']':cs)
            _   -> rec
  where
    rec@(recExp, recRest) = parse cs

    app x = first (x:) rec

-- runs parser
runParse :: String -> BFExp
runParse str
  | null rest = exp
  | otherwise = error "Unexpected \"]\" while parsing"
  where
    (exp, rest) = parse str

-- Initial tape, set to all zeroes
zeroTape :: Tape Int
zeroTape = T (repeat 0) 0 (repeat 0)

-- the state of the program
type State = (BFExp, Tape Int)

-- maps a brainfuck character to an action
action :: BFChar -> Tape Int -> IO (Tape Int)
action RIGHT tape     = return $ moveR tape
action LEFT tape      = return $ moveL tape
action INC (T l v r)  = return $ T l (v+1) r
action DEC (T l v r)  = return $ T l (v-1) r
action PRINT tape     = putChar (chr (val tape)) >> return tape
action READ (T l _ r) = getChar >>= \x -> return (T l (ord x) r)
action (BRACKET exp) tape =
  if (val tape) == 0 then
    return tape
  else do
    res <- eval (exp, tape)
    action (BRACKET exp) res

-- evaluates the code
eval :: State -> IO (Tape Int)
eval ([],x) = return x
eval (b:bs, tape) = action b tape >>= eval . (bs, )

runbf :: IO ()
runbf = do
  args <- getArgs
  if null args then
    interpreter zeroTape
  else if head args == "c" then do
    code <- readFile (args !! 1)
    writeFile "code.c" (cCodeStart ++ bfToC (runParse code) ++ "}")
  else do
    code <- readFile (head args)
    eval (runParse code, zeroTape)
    return ()

-- prompt user for codei nput
prompt :: IO String
prompt = do
    putStr "> "
    hFlush stdout
    getLine

-- interactive interpreter
interpreter :: Tape Int -> IO ()
interpreter tape = do
  print tape
  code <- prompt
  if code == "quit" || code == ":q" then
    return ()
  else do
    let parsedCode = runParse code
    if null parsedCode then do
      putStrLn "Type \"quit\" or \":q\" to quit the interpreter"
      interpreter tape
    else do
      res <- eval (runParse code, tape)
      interpreter res

cCodeStart :: String
cCodeStart = "#include <stdlib.h>\n#include <stdio.h>\nint main(){int* ptr = calloc(30000, 4);\n"

bfToC :: BFExp -> String
bfToC [] = ""
bfToC (BRACKET exp:bs) = "while(*ptr){\n" ++ bfToC exp ++ "}\n" ++ bfToC bs
bfToC (PRINT:bs) = "putchar(*ptr);\n" ++ bfToC bs
bfToC (READ:bs) = "*ptr=getchar();\n" ++ bfToC bs
bfToC (x:bs) = instr ++ show (1 + length xs) ++ ";\n" ++ rest
  where
    (xs, bs') = break (/= x) bs
    rest = bfToC bs'

    instr = case x of
      RIGHT -> "ptr+="
      LEFT -> "ptr-="
      INC -> "*ptr+="
      DEC -> "*ptr-="
