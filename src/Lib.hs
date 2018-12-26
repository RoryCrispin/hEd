
module Lib where

import System.IO
import ParsEd

data State = State {
  buffer :: [String],
  position :: Int
    }

bob :: IO()
bob = do
    handle <- openFile "file.txt" ReadMode
    contents <- hGetContents handle
    let ln = lines contents
    printLoop (State ln 0)
    hClose handle

printLoop :: State -> IO ()
printLoop state = do
  input <- getLine
  let (newState, out) = evaluate (ee input) state
    in do
       putStrLn out
       printLoop newState

evaluate :: Command -> State -> (State, String)
evaluate Command {op=Print, target=t} st =
  (st, (unlines (getTarget (buffer st) t)))

-- evaluate Command {op=Delete, target=t} st =
--   (State {
--       buffer=(deleteTarget (buffer st) t),
--       position=position st
--    }
--   , "OK")

getTarget :: [a] -> Target -> [a]
getTarget xs (topLoc, bottomLoc) =
     let top = locationToLine topLoc in
     let bottom = locationToLine bottomLoc in
       if top == bottom then
       [xs !! top]
       else
    take (bottom-top+1) $ drop top xs

locationToLine :: Location -> Int
locationToLine (Line x) = x

-- deleteTarget :: [String] -> Maybe Target -> [String]
-- deleteTarget _ Nothing = [""]
-- deleteTarget xs (Just (Line l)) = (deleteAt l xs)
-- deleteTarget xs (Just (Range _)) = [""]

-- deleteAt :: Int -> [String] -> [String]
-- deleteAt idx xs = lft ++ rgt
--   where (lft, (_:rgt)) = splitAt idx xs
