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
    printLoop (State ln 0 emptyRegTable NormalMode "")
    hClose handle

emptyState = (State [] 0 emptyRegTable NormalMode "")

printLoop :: State -> IO ()
printLoop st = do
    input <- getLine
    case mode st of
        NormalMode -> let rv = ee input st in
                        case rv of
                            Right (cmd, newParserState) -> let (newState, out) = evaluate cmd newParserState in
                                          do putStrLn out
                                             printLoop newState
                            Left err -> do putStrLn err
                                           printLoop st
        InsertMode -> case tokenizeInsertMode input of
                          Just str -> printLoop (insertLine str st)
                          Nothing -> printLoop
                                       st {mode = NormalMode}
