module State where

import qualified Data.Map as Map

type RegTable = Map.Map Char Target

data HedMode = NormalMode | InsertMode
              deriving Show

data State = State {
  buffer :: [String],
  position :: Int,
  registers :: RegTable,
  mode :: HedMode
  } deriving Show

data Location = Line Int
              deriving (Read, Show, Eq)

-- Ranges are always a tuple with a top and bottom pointer.
-- for single selections, the top == bottom.
-- This should make things easier to reason about
type Target = (Location, Location)

mkTarget :: Location -> Target
mkTarget x = (x, x)

-- Register Table Bits
updateReg key val table = Map.insert key val table

emptyRegTable = Map.empty

currentPosition :: State -> Target
currentPosition st = mkTarget (Line (position st))

lookupReg :: Char -> State -> Maybe Target
lookupReg '.' st = Just (currentPosition st)
lookupReg '$' st = Just (mkTarget (Line ((length (buffer st))-1)))
lookupReg key st = Map.lookup key (registers st)

-- Buffer manipulation
insertLine :: String -> State -> State
insertLine str st = let newBuffer =
                          insertAt (position st) str (buffer st) in
                      State {
  buffer=newBuffer
  , position=(position st) +1
  , registers=registers st
  , mode=mode st
  }

insertAt pos new_element list =
  let (ys,zs) = splitAt pos list in ys ++ [new_element] ++ zs
