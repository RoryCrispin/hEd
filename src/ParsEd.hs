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


-- I think TokOp and TokIdent is wrong - it should just be TokAlpha and we can determine
-- what it is in the syntax
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
  | c `elem` "Qpcdkain" = TokOp (operator c) : tokenize cs
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

-- TODO RC next: a parseTokenLoc may return a full location..
-- see failing test for the , case where , represents first through last lines of the document
parseTokenLoc :: Token -> State -> Maybe Location
parseTokenLoc (TokNum n) _ = Just (Line n)
parseTokenLoc (TokIdent i) st = lookupReg i st


parseTarget :: [Token] -> State -> Either (Target, [Token]) String

-- A single comma represnts the whole buffer
parseTarget (TokKey Comma : TokOp x : xs) st = Left (fullBufferTarget st, TokOp x : xs)

parseTarget (a : TokKey Comma : b : xs) st =
  case parseTokenLoc a st of
    Just loc_a ->
      case parseTokenLoc b st of
        Just loc_b -> Left ((loc_a, loc_b), xs)
        Nothing -> Right "Invalid right hand target"
    Nothing -> Right "Invalid left hand target"

parseTarget (a : TokOp op : xs) st =
  case parseTokenLoc a st of
    Just loc -> Left (mkTarget loc, TokOp op : xs)
    Nothing -> Right "Invalid target"

-- Base case of no target = current line
parseTarget (TokIdent i : xs) st = case parseTokenLoc (TokIdent i) st of
                                     Just loc -> Left ((mkTarget loc), xs)
                                     Nothing -> Right "Invalid target"
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
