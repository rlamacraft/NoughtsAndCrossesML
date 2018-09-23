module GamePlay (Move(Move), MoveResult(Success, AlreadyPlayed, AlreadyWon), makeMove, GameLog, GridIndex) where

import GameState

type GridIndex = (Int, Int)
data Move = Move Mark GridIndex

data MoveResult a = Success a
                  | AlreadyPlayed Int Int Mark
                  | AlreadyWon Mark (Grid CellState)
                  deriving Show

instance Functor MoveResult where
  fmap f (Success x) = Success (f x)
  fmap f (AlreadyPlayed i j m) = AlreadyPlayed i j m
  fmap f (AlreadyWon m x) = AlreadyWon m x

instance Applicative MoveResult where
  pure x = Success x
  Success f <*> something = fmap f something
  (AlreadyPlayed i j m) <*> _ = (AlreadyPlayed i j m)
  (AlreadyWon m x) <*> _ = (AlreadyWon m x)

instance Monad MoveResult where
  return x = Success x
  Success x >>= f = f x
  AlreadyPlayed i j m >>= f = AlreadyPlayed i j m
  AlreadyWon m x >>= f = AlreadyWon m x

makeMove :: Move -> GameState -> MoveResult GameState
makeMove (Move mark (i, j)) (State state currentWinner) = case currentWinner of
  Just x -> AlreadyWon x state
  Nothing -> case state !! i !! j of
    Played x -> AlreadyPlayed i j x
    Unplayed -> Success (updateState (Move mark (i, j)) (State state currentWinner) )
  --TODO: already won should return the end game state

updateState :: Move -> GameState -> GameState
updateState (Move mark (i, j)) (State state _) = State newGrid (winner newGrid) where
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