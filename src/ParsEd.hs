module ParsEd where
 -- TODO rc use either correctly. Right is for success
import Data.Char
import State
import Data.List
import Text.Regex.TDFA


data Operator = QuitUnconditionally
  | After
  | Change
  | Delete
  | Goto
  | Insert
  | Join
  | Mark
  | Move
  | Number
  | Print
  | Transfer
  deriving (Show, Eq)

data Keyword = Comma
  | Semicolon
             deriving (Show, Eq)

data Token = TokNum Int
  | TokReg Char
  | TokKey Keyword
  | TokChar Char
  | TokOffset Int
  | TokRegexp String
  deriving (Show, Eq)

operator :: Char -> Maybe Operator
operator c
  | c == 'a' = Just After
  | c == 'c' = Just Change
  | c == 'd' = Just Delete
  | c == 'i' = Just Insert
  | c == 'j' = Just Join
  | c == 'k' = Just Mark
  | c == 'm' = Just Move
  | c == 'n' = Just Number
  | c == 'p' = Just Print
  | c == 't' = Just Transfer
  | c == 'Q' = Just QuitUnconditionally
  | otherwise = Nothing

keyword :: Char -> Maybe Keyword
keyword k  | k == ',' = Just Comma
           | otherwise = Nothing

tokenize :: String -> [Token]
tokenize [] = []
tokenize  ('/': cs) = parseRegexp cs
tokenize  ('+' : c : cs) = if isDigit c then numberOffset c cs else TokOffset 1 : tokenize  (c : cs)
tokenize  ('-' : c : cs) = if isDigit c then numberOffset '-' (c : cs) else TokOffset (-1) : tokenize  (c : cs)
tokenize ('\'' : c : cs) = TokReg c : tokenize cs
tokenize (c : cs)
  | c `elem` "," = TokKey Comma : tokenize cs
  | c `elem` ";" = TokKey Semicolon : tokenize cs
  | isDigit c = number c cs
  | isSpace c = tokenize cs
  | otherwise = TokChar c : tokenize cs

tokenizeInsertMode :: String -> Maybe String
tokenizeInsertMode ('.' : _) = Nothing
tokenizeInsertMode xs = Just xs

parseRegexp :: String -> [Token]
parseRegexp cs =
  let (regexpStr, extras) = span regexpChar cs in
    (TokRegexp regexpStr) : tokenize (tail extras)
  where regexpChar = \x -> x /= '/' -- todo rc fix escaping regexes


-- There is a pattern here.. Go and revise the solution
number :: Char -> String -> [Token]
number c cs =
  let (digs, cs') = span isDigit cs in
    TokNum (read (c : digs)) : tokenize cs'

numberOffset :: Char -> String -> [Token]
numberOffset c cs =
  let (digs, cs') = span isDigit cs in
    TokOffset (read (c : digs)) : tokenize cs'
-- End pattern


data Command = Command {
  target :: Target,
  op :: Operator,
  params :: [Token]
  } deriving (Show, Eq)

cmdParser :: (Target, [Token]) -> Either Command String
cmdParser (r, TokChar o : xs) = case operator o of
  Just oper -> Left $ Command r oper xs
  Nothing -> Right "Invalid command"
cmdParser (r, []) = Left $ Command r Goto []

              -- TODO some debugging prints here to remove
cmdParser x = Right ("Invalid command" ++ show (fst x) ++ show (snd x))

-- TODO RC next: a parseTokenLoc may return a full location..
-- see failing test for the , case where , represents first through last lines of the document
parseTokenLoc :: Token -> State -> Maybe Location
-- parseTokenLoc (TokPlus)
parseTokenLoc (TokOffset n) st = Just (Line (position st + n))
parseTokenLoc (TokNum n) _ = Just (Line n)
parseTokenLoc (TokReg r) st = lookupReg r st
parseTokenLoc (TokChar i) st = lookupReg i st
parseTokenLoc _ _ = Nothing


parseTarget :: [Token] -> State -> Either (Target, [Token]) String

parseTarget ((TokRegexp s): xs) st = case (findRegexTarget s st) of
  Left tgt -> Left (tgt, xs)
  Right err -> Right err
-- A single comma represnts the whole buffer
parseTarget (TokKey Comma : xs) st = Left (fullBufferTarget st, xs)
parseTarget (TokKey Semicolon : xs) st = Left (currentThroughLastTarget st, xs)

parseTarget (a : TokKey Comma : b : xs) st =
  case parseTokenLoc a st of
    Just loc_a ->
      case parseTokenLoc b st of
        Just loc_b -> Left ((loc_a, loc_b), xs)
        Nothing -> Right "Invalid right hand target"
    Nothing -> Right "Invalid left hand target"

parseTarget (a :  xs) st =
  case parseTokenLoc a st of
    Just loc -> Left (mkTarget loc, xs)
    Nothing -> Right "Invalid target"

findRegexTarget :: String -> State -> Either Target String
findRegexTarget re st = case findIndex (=~ re) fwd of
  Just i -> Left $  mkTarget $ Line (i + (position st))
  Nothing -> case findIndex (=~ re) back of
    Just i -> Left $ mkTarget $ Line i
    Nothing -> Right "Target not found" -- handle invalid regex?
  where (back, fwd) = splitAt ((position st) + 1) (buffer st)

-- Base case of no target = current line
-- parseTarget (TokIdent i : xs) st = case parseTokenLoc (TokIdent i) st of
--                                      Just loc -> Left (mkTarget loc, xs)
--                                      Nothing -> Right "Invalid target"
-- parseTarget xs st = Left (mkTarget (Line (position st)), xs)


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


-- get rid of this
identToStr (TokChar n) = n
