module Eval where

import State
import ParsEd

-- TODO is this dead code? Does the parser need to update the state or just see it?
-- Updates a State with new values from the ParserState result
apn = ("n " ++)

alle :: [String] -> Int -> [String]
alle [] _ = []
alle (x:xs) n = (show n ++ " " ++ x) : alle xs (n + 1)

-- Dont really want to have to repeat this application for every function
-- this should be an applicative, I think
evaluate :: Command -> State -> (State, String)
evaluate Command {op = Print, target = t} st =
  case getTarget (buffer st) t of
    Nothing -> (st, "Invalid target")
    Just tgt -> (st, unlines tgt)

evaluate Command {op=Number, target=t} st =
  case getTarget (alle (buffer st) 0) t of
    Nothing -> (st, "Invalid target")
    Just tgt -> (st, unlines tgt)

evaluate Command {op=Delete, target=t} st =
  (State{buffer = deleteTarget (buffer st) t, position = position st,
         registers = registers st, mode = mode st}
  , "OK")

evaluate Command {op=Mark, target=t, params=p}
  State {buffer=b, position=pos, registers=r} =
  let newRegisters = updateReg (identToStr (head p)) (snd t) r in
  ((State {
       buffer=b
       , position=pos
       , registers=newRegisters
       , mode=NormalMode
       }), "OK")

-- The Goto command switches the current file position to the given
-- location, and prints that line to stdout
evaluate Command {op=Goto, target=tgt}
  State {buffer=b, position=pos, registers=r} =
  let newPosition = fst tgt in
    case getTarget b (mkTarget newPosition) of
    Nothing -> ((State {
                          buffer=b
                          , position=pos
                          , registers=r
                          , mode=NormalMode
                          }),
                       "Invalid target") -- Error
    Just targetLineContents ->
      ((State {
           buffer=b
           , position=locationToLine newPosition
           , registers=r
           , mode=NormalMode
           }),
        head targetLineContents)


evaluate Command {op=After} st =
  (State {
      buffer=buffer st
      , position=position st + 1
      , registers=registers st
      , mode=InsertMode
      },
    ">") -- TODO remove this str

evaluate Command {op=Insert} st =
  (State{buffer = buffer st, position = position st,
         registers = registers st, mode = InsertMode},
    ">") -- TODO remove this str

evaluate Command {op=QuitUnconditionally} st = error "TODO quit gracefully"


