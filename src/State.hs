{-# LANGUAGE OverloadedStrings #-}

module State where

import qualified Data.Map as Map
import qualified Data.Text as T
import Control.Lens



dSt :: State
dSt = State ["l1", "l2", "l3", "l4"] 0 (Map.fromList [('a', Line 1), ('b', Line 2), ('p', Line 2)]) NormalMode ""

data HedMode = NormalMode | InsertMode
              deriving (Show, Eq)

data State = State {
  buffer :: [T.Text],
  position :: Int,
  registers :: RegTable,
  mode :: HedMode,
  lastRegex :: String
  } deriving (Show, Eq)

newtype Location = Line Int
                     deriving (Read, Show, Eq)

type RegTable = Map.Map Char Location

-- Ranges are always a tuple with a top and bottom pointer.
-- for single line selections, the top == bottom.
-- This should make things easier to reason about
type Target = (Location, Location)


mkTarget :: Location -> Target
mkTarget x = (x, x)

fullBufferTarget :: State -> Target
fullBufferTarget s = (Line 0, Line (length (buffer s) -1))

currentThroughLastTarget :: State -> Target
currentThroughLastTarget s = (Line (position s), Line (length (buffer s) -1))

-- Register Table Bits
updateReg key val table = Map.insert key val table

emptyRegTable = Map.empty

--currentPosition :: State -> Target
--currentPosition st = mkTarget (Line (position st))

lookupReg :: Char -> State -> Maybe Location
lookupReg '.' st = Just (Line (position st))
lookupReg '$' st = Just (Line (length (buffer st) - 1)) -- The last line
lookupReg key st = Map.lookup key (registers st)

-- Buffer manipulation
-- insertLine :: T.Text -> State -> State
insertLine str st = let newBuffer =
                          insertAt (position st) str (buffer st) in
                      st {buffer=newBuffer , position=position st + 1}

insertAt pos new_element list =
  let (ys,zs) = splitAt pos list in ys ++ [new_element] ++ zs

-- getTarget :: [a] -> Target -> Maybe [a]
getTarget xs (topLoc, bottomLoc) =
  let top = locationToLine topLoc in
    let bottom = locationToLine bottomLoc in
      if top == bottom then
        liftList (xs ^? element top)
      else
        Just (take (bottom-top+1) $ drop top xs)

locationToLine :: Location -> Int
locationToLine (Line x) = x

targetToLines :: Target -> (Int, Int)
targetToLines (Line a, Line b) = (a, b)

deleteTarget :: [a] -> Target  -> [a]
deleteTarget xs (topLoc, bottomLoc) =
  let top = locationToLine topLoc in
    let bottom = locationToLine bottomLoc in
      take top xs ++ drop ( bottom+1 ) xs

-- TODO there must be a better way
liftList :: Maybe a -> Maybe [a]
liftList Nothing = Nothing
liftList (Just a) = Just [a]
