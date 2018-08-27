module GamePlay (Move(Move), MoveResult(Success, AlreadyPlayed, AlreadyWon), makeMove, GameLog, GridIndex) where

import GameState

type GridIndex = (Int, Int)
data Move = Move Mark GridIndex

data MoveResult = Success GameState
                | AlreadyPlayed Int Int Mark
                | AlreadyWon Mark
                deriving Show

makeMove :: GameState -> Move -> MoveResult
makeMove (State state currentWinner) (Move mark (i, j)) = case currentWinner of
  Just x -> AlreadyWon x
  Nothing -> case state !! i !! j of
    Played x -> AlreadyPlayed i j x
    Unplayed -> Success (updateState (State state currentWinner) (Move mark (i, j)))

updateState :: GameState -> Move -> GameState
updateState (State state _) (Move mark (i, j)) = State newGrid (winner newGrid) where
  before = take i state
  after = drop (i + 1) state
  row = updateRow (state !! i) mark j
  newGrid = before ++ [row] ++ after

updateRow :: [CellState] -> Mark -> Int -> [CellState]
updateRow row mark j = before ++ [Played mark] ++ after where
  before = take j row
  after = drop (j + 1) row

-- 2 agents repeated playing the game
-- in main, we simply call a function "play n games"
-- playGames is a recursive function that plays a game, learns from it, and repeats, until n == 0
-- playGame takes two strategies and loops until either one agent wins or board is full
-- it then returns a log of all of the moves made, with the state of the board when the move was made
-- playGames then learns from this by adjusting the weightings on the strategy for both agents

type GameLog = [(GameState, Move)] --TODO: For learning