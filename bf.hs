{-# LANGUAGE TupleSections #-}

import Data.Char
import Data.Bifunctor
import System.Environment
import Tape

-- Data type for brainfuck character
-- treats expressions in between brackets as a single character
data BFChar = INC
            | DEC
            | ADD
            | SUB
            | PRINT
            | READ
            | BRACKET BFExp
            deriving (Show)

-- a brainfuck expression is just a list of brainfuck characters
type BFExp = [BFChar]

-- code parser
-- need to figure '[' with unmatched ']'
parse :: String -> (BFExp, String)
parse []     = ([],[])
parse (c:cs) = 
  case c of '>' -> app INC
            '<' -> app DEC
            '+' -> app ADD
            '-' -> app SUB
            '.' -> app PRINT
            ',' -> app READ
            '[' -> first (BRACKET recExp:) (parse recRest)
            ']' -> ([],cs)
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
action INC tape       = return $ moveR tape
action DEC tape       = return $ moveL tape
action ADD (T l v r)  = return $ T l (v+1) r
action SUB (T l v r)  = return $ T l (v-1) r
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

main :: IO ()
main = do
  args <- getArgs
  code <- readFile (head args)
  eval (runParse code, zeroTape)
  return ()
