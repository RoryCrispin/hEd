module State where

import qualified Data.Map as Map
import Control.Lens


data HedMode = NormalMode | InsertMode
              deriving (Show, Eq)

data State = State {
  buffer :: [String],
  position :: Int,
  registers :: RegTable,
  mode :: HedMode
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
fullBufferTarget s = (Line 0, Line (length (buffer s)))

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
insertLine :: String -> State -> State
insertLine str st = let newBuffer =
                          insertAt (position st) str (buffer st) in
                      State {
  buffer=newBuffer
  , position=position st + 1
  , registers=registers st
  , mode=mode st
  }

insertAt pos new_element list =
  let (ys,zs) = splitAt pos list in ys ++ [new_element] ++ zs

getTarget :: [a] -> Target -> Maybe [a]
getTarget xs (topLoc, bottomLoc) =
  let top = locationToLine topLoc in
    let bottom = locationToLine bottomLoc in
      if top == bottom then
        liftList (xs ^? element top)
      else
        Just (take (bottom-top+1) $ drop top xs)

locationToLine :: Location -> Int
locationToLine (Line x) = x


deleteTarget :: [a] -> Target  -> [a]
deleteTarget xs (topLoc, bottomLoc) =
  let top = locationToLine topLoc in
    let bottom = locationToLine bottomLoc in
      take top xs ++ drop ( bottom+1 ) xs

-- TODO there must be a better way
liftList :: Maybe a -> Maybe [a]
liftList Nothing = Nothing
liftList (Just a) = Just [a]
