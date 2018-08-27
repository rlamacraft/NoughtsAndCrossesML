module GameState (Mark(X, O), CellState(Played, Unplayed), Grid, GameState(State), areIsomorphic, winner, mockState) where

import Data.List
import Control.Applicative

data Mark = X | O deriving (Eq, Show)

data CellState = Played Mark | Unplayed deriving Eq

instance Show CellState where
  show (Played a) = show a
  show Unplayed = "_"

type Grid a = [[a]]
data GameState = State (Grid CellState) (Maybe Mark) deriving Eq
  --Maybe Mark here denotes the winner

instance Show GameState where
  show (State grid _) = showGrid grid

showGrid :: Grid CellState -> String
showGrid [] = ""
showGrid (x:[]) = showGridRow x
showGrid (x:xs) = showGridRow x ++ "\n---+---+---\n" ++ showGrid xs

showGridRow :: [CellState] -> String
showGridRow [] = ""
showGridRow (x:[]) = " " ++ show x
showGridRow (x:xs) = " " ++ show x ++ " |" ++ showGridRow xs

---------------------
-- Isomorpic States
---------------------

areIsomorphic :: GameState -> GameState -> Bool
areIsomorphic left right = left `elem` (isomorphicSet right)

isomorphicSet :: GameState -> [GameState]
isomorphicSet state = map ($ state) transformations where
  transformations = [
    id,
    flipGrid Vertical,
    flipGrid Horizontal,
    flipGrid TopLeftToBottomRight,
    flipGrid TopRightToBottomLeft,
    (flipGrid Vertical) . (flipGrid Horizontal),
    (flipGrid Vertical) . (flipGrid TopLeftToBottomRight),
    (flipGrid Vertical) . (flipGrid TopRightToBottomLeft) ]

data LineOfSymmetry
  = Vertical
  | Horizontal
  | TopLeftToBottomRight
  | TopRightToBottomLeft

flipGrid :: LineOfSymmetry -> GameState -> GameState
flipGrid Vertical = flipRows
flipGrid Horizontal = transposeGrid . flipGrid Vertical . transposeGrid
flipGrid TopLeftToBottomRight = transposeGrid
flipGrid TopRightToBottomLeft = transposeGrid . flipGrid Vertical . flipGrid Horizontal

flipRows :: GameState -> GameState
flipRows (State rows winner) = State (map reverse rows) winner

transposeGrid :: GameState -> GameState
transposeGrid (State rows winner) = State (transpose rows) winner

------------------
-- Win Condition
------------------

winner :: Grid CellState -> Maybe Mark
winner state = foldr1 (<|>) $ map ($ state) [
  winnerByRow,
  winnerByColumn,
  winnerByLeftRightDiagonal,
  winnerByRightLeftDiagonal ]

winnerByRow :: Grid CellState -> Maybe Mark
winnerByRow state = foldr1 (<|>) $ map rowOfPlayedMark state

winnerByColumn :: Grid CellState -> Maybe Mark
winnerByColumn = winnerByRow . transpose

winnerByLeftRightDiagonal :: Grid CellState -> Maybe Mark
winnerByLeftRightDiagonal grid = rowOfPlayedMark [grid !! 0 !! 0, grid !! 1 !! 1, grid !! 2 !! 2]

winnerByRightLeftDiagonal :: Grid CellState -> Maybe Mark
winnerByRightLeftDiagonal grid = rowOfPlayedMark [grid !! 0 !! 2, grid !! 1 !! 1, grid !! 2 !! 0]

rowOfPlayedMark :: [CellState] -> Maybe Mark
rowOfPlayedMark state
  | rowOfSameMark state = case state !! 0 of
                            Played a -> Just a
                            Unplayed -> Nothing
  | otherwise = Nothing

rowOfSameMark :: [CellState] -> Bool
rowOfSameMark (x:y:xs) = x == y && rowOfSameMark (y:xs)
rowOfSameMark _ = True

----------
-- Utils
----------

mockState :: GameState
mockState = State [[Played X, Unplayed, Unplayed], [Played O, Unplayed, Unplayed], [Unplayed, Unplayed, Unplayed]] Nothing

mockState_winner :: GameState
mockState_winner = State [[Played X, Unplayed, Played O], [Played X, Played O, Unplayed], [Played X, Unplayed, Unplayed]] (Just X)