module GamePlay () where

import GameState

type Move = (Int, Int)

data MoveResult = Success GameState
                | AlreadyPlayed Int Int Mark deriving Show

makeMove :: GameState -> Mark -> Move -> MoveResult
makeMove (State state) mark (i, j) = case state !! i !! j of
  Played x -> AlreadyPlayed i j x
  Unplayed -> Success (updateState (State state) mark (i, j))

updateState :: GameState -> Mark -> Move -> GameState
updateState (State state) mark (i, j) = State (before ++ [row] ++ after) where
  before = take i state
  after = drop (i + 1) state
  row = updateRow (state !! i) mark j

updateRow :: [CellState] -> Mark -> Int -> [CellState]
updateRow row mark j = before ++ [Played mark] ++ after where
  before = take j row
  after = drop (j + 1) row