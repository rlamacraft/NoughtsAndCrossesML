module Main (main) where

import System.Random (getStdRandom, randomR)
import Data.Sequence (mapWithIndex, fromList)
import Data.Foldable (toList)
import GameState (startGameState, Mark(X, O), GameState(State), isPlayed, CellState)
import GamePlay (makeMove, Move(Move), MoveResult(Success, AlreadyPlayed, AlreadyWon), GridIndex)

getRandomMoves :: Int -> IO [Int]
getRandomMoves n = sequence $ replicate n (getStdRandom (randomR (0,8)) :: IO Int)

getRandomisedMoveSelection :: Int -> IO [Int]
getRandomisedMoveSelection k = sequence $ getRandomisedMoveSelection' k where
  getRandomisedMoveSelection' 0 = []
  getRandomisedMoveSelection' k = (getStdRandom (randomR (1,k-1)) :: IO Int) : getRandomisedMoveSelection' (k - 1)

getPlayableCells :: GameState -> [GridIndex]
getPlayableCells (State cells _) = getPlayableCells' cells where
  getPlayableCells' :: [[CellState]] -> [GridIndex]
  getPlayableCells' states = returnJustGridIndexes $ filterUnplayed $ calculateGridIndexes states
  calculateGridIndexes :: [[CellState]] -> [(GridIndex, CellState)]
  -- calculateGridIndexes = fromList . mapWithIndex (\i -> fromList . mapWithIndex (\j cell -> ((i,j),cell)) . toList) . toList . concat
  calculateGridIndexes states = concat $ toList $ mapWithIndex (\i row -> toList $ mapWithIndex (\j cell -> ((i,j),cell)) $ fromList row) $ fromList states
  filterUnplayed :: [(GridIndex, CellState)] -> [(GridIndex, CellState)]
  filterUnplayed = filter (\(gridIndex,cellState) -> not $ isPlayed cellState)
  returnJustGridIndexes :: [(GridIndex, CellState)] -> [GridIndex]
  returnJustGridIndexes = map (\(gridIndex,cellState) -> gridIndex)

-- assumes index < number of playableCells, ahh Idris
makeRandomisedMove :: Mark -> Int -> GameState -> MoveResult GameState
makeRandomisedMove mark index gameState = makeMove (Move mark (i, j)) gameState where
  (i,j) = getPlayableCells gameState !! index

playRandomisedGame :: [Int] -> MoveResult GameState
playRandomisedGame xs = return startGameState
                      >>= makeRandomisedMove X (xs !! 0)
                      >>= makeRandomisedMove O (xs !! 1)
                      >>= makeRandomisedMove X (xs !! 2)
                      >>= makeRandomisedMove O (xs !! 3)
                      >>= makeRandomisedMove X (xs !! 4)
                      >>= makeRandomisedMove O (xs !! 5)
                      >>= makeRandomisedMove X (xs !! 6)
                      >>= makeRandomisedMove O (xs !! 7)
                      >>= makeRandomisedMove X (xs !! 8)

data FinishedGameState = Winner Mark | Draw | Failed deriving (Show)

determineWinner :: MoveResult GameState -> FinishedGameState
determineWinner (Success (State _ Nothing)) = Draw
determineWinner (Success (State _ (Just mark))) = Winner mark
determineWinner (AlreadyPlayed _ _ _) = Failed
determineWinner (AlreadyWon mark _) = Winner mark

main :: IO ()
main = do
  xs <- getRandomisedMoveSelection 9
  print $ show $ xs

  playedGame <- return $ playRandomisedGame xs

  case playedGame of
    Success state -> putStrLn $ show state
    AlreadyPlayed _ _ mark -> putStrLn ("already played: " ++ show mark)
    AlreadyWon mark grid -> putStrLn $ "already won" ++ show mark ++ "\n" ++ show (State grid (Just mark))

  putStrLn $ show $ determineWinner playedGame
