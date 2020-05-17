module ParsEd where

import Data.Char
import State


data Operator = QuitUnconditionally
  | After
  | Change
  | Delete
  | Goto
  | Insert
  | Mark
  | Number
  | Print
  deriving (Show, Eq)

data Keyword = Comma
             deriving (Show, Eq)

data Token =
  TokOp Operator
  | TokIdent Char
  | TokNum Int
  | TokKey Keyword
  deriving (Show, Eq)

operator :: Char -> Operator
operator c
  | c == 'a' = After
  | c == 'c' = Change
  | c == 'd' = Delete
  | c == 'i' = Insert
  | c == 'k' = Mark
  | c == 'n' = Number
  | c == 'p' = Print
  | c == 'Q' = QuitUnconditionally

keyword :: Char -> Keyword
keyword k  | k == ',' = Comma

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
  | c `elem` "qpcdkain" = TokOp (operator c) : tokenize cs
  | c `elem` "," = TokKey (keyword c) : tokenize cs
  | isDigit c = number c cs
  | isSpace c = tokenize cs
  | otherwise = TokIdent c : tokenize cs

tokenizeInsertMode :: String -> Maybe String
tokenizeInsertMode ('.' : xs) = Nothing
tokenizeInsertMode xs = Just xs

number c cs =
  let (digs, cs') = span isDigit cs in
    TokNum (read (c : digs)) : tokenize cs'

data Command = Command {
  target :: Target,
  op :: Operator,
  params :: [Token]
  } deriving (Show, Eq)

cmdParser :: (Target, [Token]) -> Either Command String
cmdParser (r, TokOp o : xs) = Left $ Command r o xs
cmdParser (r, []) = Left $ Command r Goto []

              -- TODO some debugging prints here to remove
cmdParser x = Right ("Invalid command" ++ show (fst x) ++ show (snd x))


-- Parses range from input tokens and returns unconsumed tokens
parseTarget :: [Token] -> State -> Either (Target, [Token]) String
parseTarget (TokNum n : TokKey k : TokNum m : xs) _ = case k of
                                       Comma -> Left ((Line n, Line m), xs)
parseTarget (TokNum n : xs) _ = Left (mkTarget (Line n), xs)
parseTarget (TokIdent i: TokKey k : TokNum n: xs) st = case k of
   Comma -> let i_res = lookupReg i st in
              case i_res of
                  Just loc -> Left ((fst loc, Line n), xs)
                  Nothing -> Right "Invalid register"
-- parseTarget (TokIdent i : []) st = Left ((currentPosition st), [TokIdent i])
parseTarget (TokIdent i : xs) st = case lookupReg i st of
                                       Just tgt -> Left (tgt, xs)
                                       Nothing -> Right "Invalid register"

-- Base case of no target = current line
parseTarget xs st = Left (mkTarget (Line (position st)), xs)


baseParser :: [Token] -> State -> Either Command String
baseParser xs st =
  case parseTarget xs st of
    Left (tgt, tkns) -> case cmdParser (tgt, tkns) of
                            Left cmd -> Left cmd
                            Right err -> Right err
    Right err -> Right err

-- TODO do we need to return State or is it readonly
ee :: String -> State -> (Either Command String, State)
ee s st = (baseParser (tokenize s) st, st)



identToStr (TokIdent n) = n
