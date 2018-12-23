module ParsEd where

import Data.Char

data Operator = Quit | Print | Change | Delete
    deriving (Show, Eq)

data Token = TokOp Operator
           | TokIdent String
           | TokNum Int
    deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == 'q' = Quit
           | c == 'p' = Print
           | c == 'c' = Change
           | c == 'd' = Delete

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
    | elem c "qpcd" = TokOp (operator c) : tokenize cs
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
            | Range (Int, Int)
            deriving (Read, Show, Eq)

data Command = Command {op :: Operator,
                        target :: Maybe Target}
               deriving (Show, Eq)

x = [TokOp Print, TokNum 4]

parser :: [Token] -> Command
parser (TokOp o: TokNum n: xs) = Command o (Just (Line n))
parser (TokOp o : xs) = Command o Nothing

ee :: String -> Command
ee s = parser $ tokenize s
