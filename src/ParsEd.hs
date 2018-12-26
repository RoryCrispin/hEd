module ParsEd where

import Data.Char

data Operator = Quit | Print | Change | Delete
    deriving (Show, Eq)

data Keyword = Comma
    deriving (Show, Eq)

data Token = TokOp Operator
           | TokIdent String
           | TokNum Int
           | TokKey Keyword
    deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == 'q' = Quit
           | c == 'p' = Print
           | c == 'c' = Change
           | c == 'd' = Delete

keyword :: Char -> Keyword
keyword k  | k == ',' = Comma

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
    | elem c "qpcd" = TokOp (operator c) : tokenize cs
    | elem c "," = TokKey (keyword c) : tokenize cs
    | isDigit c = number c cs
    | isAlpha c = identifier c cs
    | isSpace c = tokenize cs
    | otherwise = error $ "Cannot tokenize " ++ [c]

identifier c cs = let (str, cs') = span isAlphaNum cs in
                  TokIdent (c:str) : tokenize cs'

number c cs =
   let (digs, cs') = span isDigit cs in
   TokNum (read (c : digs)) : tokenize cs'

data Target = Line Int
            | Register String
            deriving (Read, Show, Eq)

-- Ranges are always a tuple with a top and bottom pointer.
-- for single selections, the top == bottom.
-- This should make things easier to reason about
type Range = (Target, Target)

mkRange :: Target -> Range
mkRange x = (x, x)

data Command = Command {
  range :: Range,
  op :: Operator
  } deriving (Show, Eq)

cmdParser :: (Range, [Token]) -> Command
cmdParser (r, (TokOp o : xs)) = Command (r) o

-- Parses range from input tokens and returns unconsumed tokens
parseTarget :: [Token] -> (Range, [Token])
parseTarget (TokNum n : TokKey k : TokNum m : xs) = case k of
     Comma -> ((Line n, Line m), xs)
parseTarget (TokNum n : xs) = (mkRange (Line n), xs)
-- parseTarget (TokIdent n : xs) = (mkRange (Register n), xs)
-- TODO: Register support.. would be nice for $kk to be more polymorphic

baseParser :: [Token] -> Command
baseParser xs = let (tgt, tkns) = parseTarget xs in
  cmdParser (tgt, tkns)

ee :: String -> Command
ee s = baseParser $ tokenize s
