module ParsEd where

import Data.Char
import qualified Data.Map as Map

type RegTable = Map.Map Char Target

data ParserState = ParserState {
  ps_position :: Int,
  ps_registers :: RegTable
  } deriving Show

data Operator = Quit | Print | Change | Delete | Mark | Goto
              deriving (Show, Eq)

data Keyword = Comma
             deriving (Show, Eq)

data Token = TokOp Operator
           | TokIdent Char
           | TokNum Int
           | TokKey Keyword
           deriving (Show, Eq)

operator :: Char -> Operator
operator c | c == 'q' = Quit
           | c == 'p' = Print
           | c == 'c' = Change
           | c == 'd' = Delete
           | c == 'k' = Mark

keyword :: Char -> Keyword
keyword k  | k == ',' = Comma

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
  | elem c "qpcdk" = TokOp (operator c) : tokenize cs
  | elem c "," = TokKey (keyword c) : tokenize cs
  | isDigit c = number c cs
  | isSpace c = tokenize cs
  | otherwise = (TokIdent c : tokenize cs)

number c cs =
  let (digs, cs') = span isDigit cs in
    TokNum (read (c : digs)) : tokenize cs'

data Location = Line Int
              deriving (Read, Show, Eq)

-- Ranges are always a tuple with a top and bottom pointer.
-- for single selections, the top == bottom.
-- This should make things easier to reason about
type Target = (Location, Location)

mkTarget :: Location -> Target
mkTarget x = (x, x)

data Command = Command {
  target :: Target,
  op :: Operator,
  params :: [Token]
  } deriving (Show, Eq)

cmdParser :: (Target, [Token]) -> Either Command String
cmdParser (r, (TokOp o : xs)) = Left $ Command r o xs
cmdParser (r, []) = Left $ Command r Goto []

              -- TODO some debugging prints here to remove
cmdParser x = Right ("Invalid command" ++ (show  (fst x)))


-- Parses range from input tokens and returns unconsumed tokens
parseTarget :: [Token] -> ParserState -> Either (Target, [Token]) String
parseTarget (TokNum n : TokKey k : TokNum m : xs) _ = case k of
                                       Comma -> Left ((Line n, Line m), xs)
parseTarget (TokNum n : xs) _ = Left (mkTarget (Line n), xs)

parseTarget (TokIdent i : xs) ps = case lookupReg i ps of
                                     Nothing -> Right "Invalid register"
                                     Just tgt -> Left (tgt, xs)
-- Base case of no target = current line
parseTarget xs ps = Left (mkTarget (Line (ps_position ps)), xs)


baseParser :: [Token] -> ParserState -> Either Command String
baseParser xs ps = case parseTarget xs ps of
                     Left (tgt, tkns) -> case (cmdParser (tgt, tkns)) of
                                         Left cmd -> Left cmd
                                         Right err -> Right err
                     Right err -> Right err

-- TODO do we need to return ParserState or is it readonly
ee :: String -> ParserState -> (Either Command String, ParserState)
ee s ps = (baseParser (tokenize s) ps, ps)

-- Register Table Bits
updateReg key val table = Map.insert key val table

emptyRegTable = Map.empty

lookupReg :: Char -> ParserState -> Maybe Target
lookupReg '.' ps = Just (mkTarget ( Line (ps_position ps)))
lookupReg key ps = Map.lookup key (ps_registers ps)
