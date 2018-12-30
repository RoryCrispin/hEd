
module Lib where

import System.IO
import ParsEd

-- Debug values

ts = State ["l1", "l2"] 0 emptyRegTable

bob :: IO()
bob = do
    handle <- openFile "file.txt" ReadMode
    contents <- hGetContents handle
    let ln = lines contents
    printLoop (State ln 0 (emptyRegTable))
    hClose handle

printLoop :: State -> IO ()
printLoop st = do
    input <- getLine
    let (parsedCmd, newParserState)= (ee input (st)) in
      case parsedCmd of
      Left cmd -> let
        (newState, out) = evaluate cmd newParserState in
          do
            putStrLn out
            printLoop newState
      Right err -> do
        putStrLn err
        printLoop st

-- TODO is this dead code? Does the parser need to update the state or just see it?
-- Updates a State with new values from the ParserState result

evaluate :: Command -> State -> (State, String)
evaluate Command {op=Print, target=t} st =
  (st, (unlines (getTarget (buffer st) t)))

evaluate Command {op=Delete, target=t} st =
  (State {
      buffer=(deleteTarget (buffer st) t),
      position=position st,
      registers=registers st
   }
  , "OK")

evaluate Command {op=Mark, target=t, params=p}
  State {buffer=b, position=pos, registers=r} =
  let newRegisters = updateReg (identToStr (head p)) t r in
  ((State {buffer=b,
           position=pos,
           registers=newRegisters}), "OK")

-- The Goto command switches the current file position to the given
-- location, and prints that line to stdout
evaluate Command {op=Goto, target=tgt}
  State {buffer=b, position=pos, registers=r} =
  let newPosition = fst tgt in
    let targetLineContents = getTarget b (mkTarget newPosition) in
      ((State {buffer=b,
               position=locationToLine newPosition,
               registers=r}), head targetLineContents)

evaluate Command {op=Quit} st = error "TODO quit gracefully"

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

identToStr (TokIdent n) = n

deleteTarget :: [a] -> Target  -> [a]
deleteTarget xs (topLoc, bottomLoc) =
  let top = locationToLine topLoc in
    let bottom = locationToLine bottomLoc in
      take top xs ++ drop ( bottom+1 ) xs

