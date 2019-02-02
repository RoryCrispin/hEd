module Lib where

import System.IO
import ParsEd
import State
import Eval

bob :: IO()
bob = do
    handle <- openFile "shakespeare.txt" ReadMode
    contents <- hGetContents handle
    let ln = lines contents
    printLoop (State ln 0 (emptyRegTable) NormalMode)
    hClose handle

printLoop :: State -> IO ()
printLoop st = do
    input <- getLine
    case (mode st) of
      NormalMode ->
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
      InsertMode -> do
          case (tokenizeInsertMode input) of
            Just str -> do
              printLoop (insertLine str st)
            Nothing ->
              printLoop State {
                buffer=buffer st
                , position=position st
                , registers=registers st
                , mode=NormalMode
                }
