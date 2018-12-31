module ParsEd where

import Data.Char
import qualified Data.Map as Map

type RegTable = Map.Map Char Target

data Operator = Quit
  | Print
  | Change
  | Delete
  | Mark
  | Goto
  | After
  | Insert
  deriving (Show, Eq)

data Keyword = Comma
             deriving (Show, Eq)

data Token =
  TokOp Operator
  | TokIdent Char
  | TokNum Int
  | TokKey Keyword
  deriving (Show, Eq)

data HedMode = NormalMode | InsertMode
              deriving Show

data State = State {
  buffer :: [String],
  position :: Int,
  registers :: RegTable,
  mode :: HedMode
  } deriving Show

data Location = Line Int
              deriving (Read, Show, Eq)

operator :: Char -> Operator
operator c
  | c == 'q' = Quit
  | c == 'p' = Print
  | c == 'c' = Change
  | c == 'a' = After
  | c == 'i' = Insert
  | c == 'd' = Delete
  | c == 'k' = Mark

keyword :: Char -> Keyword
keyword k  | k == ',' = Comma

tokenize :: String -> [Token]
tokenize [] = []
tokenize (c : cs)
  | elem c "qpcdkai" = TokOp (operator c) : tokenize cs
  | elem c "," = TokKey (keyword c) : tokenize cs
  | isDigit c = number c cs
  | isSpace c = tokenize cs
  | otherwise = (TokIdent c : tokenize cs)

tokenizeInsertMode :: String -> Maybe String
tokenizeInsertMode ('.' : xs) = Nothing
tokenizeInsertMode xs = Just xs

number c cs =
  let (digs, cs') = span isDigit cs in
    TokNum (read (c : digs)) : tokenize cs'

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
cmdParser x = Right ("Invalid command" ++ (show  (fst x)) ++ (show (snd x)))


-- Parses range from input tokens and returns unconsumed tokens
parseTarget :: [Token] -> State -> Either (Target, [Token]) String
parseTarget (TokNum n : TokKey k : TokNum m : xs) _ = case k of
                                       Comma -> Left ((Line n, Line m), xs)
parseTarget (TokNum n : xs) _ = Left (mkTarget (Line n), xs)
parseTarget (TokIdent i: TokKey k : TokNum n: xs) st = case k of
   Comma -> let i_res = (lookupReg i st) in
     case i_res of
       Just loc -> Left (((fst loc), Line n), xs)
       Nothing -> Right "Invalid register"
-- parseTarget (TokIdent i : []) st = Left ((currentPosition st), [TokIdent i])
parseTarget (TokIdent i : xs) st = case (lookupReg i st) of
                                     Just tgt -> Left (tgt, xs)
                                     Nothing -> Right "Invalid register"

-- Base case of no target = current line
parseTarget xs st = Left (mkTarget (Line (position st)), xs)


baseParser :: [Token] -> State -> Either Command String
baseParser xs st =
  case parseTarget xs st of
    Left (tgt, tkns) -> case (cmdParser (tgt, tkns)) of
                          Left cmd -> Left cmd
                          Right err -> Right err
    Right err -> Right err

-- TODO do we need to return State or is it readonly
ee :: String -> State -> (Either Command String, State)
ee s st = (baseParser (tokenize s) st, st)

-- Register Table Bits
updateReg key val table = Map.insert key val table

emptyRegTable = Map.empty

currentPosition :: State -> Target
currentPosition st = mkTarget (Line (position st))

lookupReg :: Char -> State -> Maybe Target
lookupReg '.' st = Just (currentPosition st)
lookupReg '$' st = Just (mkTarget (Line ((length (buffer st))-1)))
lookupReg key st = Map.lookup key (registers st)
