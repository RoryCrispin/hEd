module Lib where

import System.IO
import ParsEd
import State
import Eval

bob :: [String] -> IO()
bob args = do
    handle <- openFile (head args) ReadMode
    contents <- hGetContents handle
    let ln = lines contents
    printLoop (State ln 0 emptyRegTable NormalMode)
    hClose handle

emptyState = (State [] 0 emptyRegTable NormalMode)

printLoop :: State -> IO ()
printLoop st = do
    input <- getLine
    case mode st of
        NormalMode -> let (parsedCmd, newParserState) = ee input st in
                        case parsedCmd of
                            Left cmd -> let (newState, out) = evaluate cmd newParserState in
                                          do putStrLn out
                                             printLoop newState
                            Right err -> do putStrLn err
                                            printLoop st
        InsertMode -> case tokenizeInsertMode input of
                          Just str -> printLoop (insertLine str st)
                          Nothing -> printLoop
                                       State{buffer = buffer st, position = position st,
                                             registers = registers st, mode = NormalMode}
