module GameState (CellState(X, O, Empty), GameState(State), areIsomorphic) where

import Data.List

data CellState = X | O | Empty deriving Eq

instance Show CellState where
  show X = "X"
  show O = "O"
  show Empty = "_"

data GameState = State [[CellState]] deriving Eq

instance Show GameState where
  show (State []) = ""
  show (State (x:[])) = showGridRow x
  show (State (x:xs)) = showGridRow x ++ "\n---+---+---\n" ++ show (State xs)

showGridRow :: [CellState] -> String
showGridRow [] = ""
showGridRow (x:[]) = " " ++ show x
showGridRow (x:xs) = " " ++ show x ++ " |" ++ showGridRow xs

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
flipRows (State rows) = State $ map reverse rows

transposeGrid :: GameState -> GameState
transposeGrid (State rows) = State $ transpose rows
