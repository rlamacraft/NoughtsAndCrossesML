module GamePlay () where

import GameState

data Move = Move Mark Int Int

data MoveResult = Success GameState
                | AlreadyPlayed Int Int Mark deriving Show

makeMove :: GameState -> Move -> MoveResult
makeMove (State state) (Move mark i j) = case state !! i !! j of
  Played x -> AlreadyPlayed i j x
  Unplayed -> Success (updateState (State state) (Move mark i j))

updateState :: GameState -> Move -> GameState
updateState (State state) (Move mark i j) = State (before ++ [row] ++ after) where
  before = take i state
  after = drop (i + 1) state
  row = updateRow (state !! i) mark j

updateRow :: [CellState] -> Mark -> Int -> [CellState]
updateRow row mark j = before ++ [Played mark] ++ after where
  before = take j row
  after = drop (j + 1) row