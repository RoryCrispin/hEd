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
  ( st {buffer = deleteTarget (buffer st) t}
  , "OK")

evaluate Command {op=Mark, target=t, params=p} st =
  let newRegisters = updateReg (identToStr (head p)) (snd t) (registers st) in
  ((st {registers=newRegisters
       , mode=NormalMode
       }), "OK")

-- The Goto command switches the current file position to the given
-- location, and prints that line to stdout
evaluate Command {op=Goto, target=tgt} st =
  let newPosition = fst tgt in
    case getTarget (buffer st) (mkTarget newPosition) of
      Nothing -> (st, "Invalid target") -- Error
      Just targetLineContents -> (
        (st {position=locationToLine newPosition , mode=NormalMode}), head targetLineContents
        )


evaluate Command {op=After, target=tgt} st =
  (st {position=(locationToLine (fst tgt)) + 1
      , mode=InsertMode}
  , ">") -- TODO remove this str

evaluate Command {op=Insert, target=tgt} st =
  (st {position = locationToLine (fst tgt)
      , mode = InsertMode}
  , ">") -- TODO remove this str

evaluate Command {op=Change, target=t} st =
  (st {
      buffer = deleteTarget (buffer st) t
      , mode = InsertMode}
  , ">")

evaluate Command {op=Join, target=t} st =
  (st {
      buffer = joinLinesTarget t (buffer st)
      , mode = NormalMode}
  , "OK")

evaluate Command {op=Move, target=t, params=p} st =
  case parseTarget p st of
    Right (dst, _, newState) -> case moveLinesTarget t (fst dst) (buffer newState) of
      Just newBuf ->
        (newState {buffer = newBuf, mode = NormalMode}
        , "OK")
      Nothing -> (st, "Invalid destination")
    Left e -> (st, e)
-- Maybe the targets in these params should be parsed properly, or evaluate should return a Maybe

evaluate Command {op=Transfer, target=t, params=p} st =
  case parseTarget p st of
    Right (dst, _, newState) -> (
      newState {
          buffer = transferLinesTarget t (fst dst) (buffer newState)
          , mode = NormalMode}
      , "OK")
    Left e -> (st, e)


evaluate Command {op=QuitUnconditionally} st = error "TODO quit gracefully"


-- fmap Location needed again..
joinLinesTarget :: Target -> [String] -> [String]
joinLinesTarget tgt = joinLines (targetToLines tgt)


joinLines :: (Int, Int) -> [[a]] -> [[a]]
joinLines (start, end) lst = take start lst ++ [concat selection] ++ drop (end+1) lst
  where selection = onlyLines (start, end) lst

moveLinesTarget tgt dst = moveLines (targetToLines tgt) (locationToLine dst)

transferLinesTarget tgt dst = transferLines (targetToLines tgt) (locationToLine dst)

moveLines :: (Int, Int) -> Int -> [a] -> Maybe [a]
moveLines (start, end) pos buf
  | pos < start = Just $ inject selection pos sansBuf
  | pos > end = Just $ inject selection (pos - (end-start)) sansBuf
  | otherwise = Nothing
  where sansBuf = withoutLines (start, end) buf
        selection = onlyLines (start, end) buf


transferLines :: (Int, Int) -> Int -> [a] -> [a]
transferLines (start, end) pos buf = inject selection pos buf
  where selection = onlyLines (start, end) buf

withoutLines :: (Int, Int) -> [a] -> [a]
withoutLines (start, end) lst = take start lst ++ drop (end+1) lst

onlyLines :: (Int, Int) -> [a] -> [a]
onlyLines (start, end) lst = (drop start (take (end+1) lst))

inject :: [a] -> Int -> [a] -> [a]
inject a pos lst = left ++ a ++ right
  where (left, right) = splitAt pos lst

