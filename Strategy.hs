module Strategy (getStrategyFromGameState, selectMove) where

import GameState
import GamePlay

import Data.Maybe
import Data.List

type MoveWeighting = Int
type PossibleMoves = Grid MoveWeighting
data Strategy = Strategy GameState PossibleMoves deriving Eq
type LearnedStrategies = [Strategy]

-----------------------
-- learnFromPriorGame
-----------------------

learnFromPriorGame :: LearnedStrategies -> GameLog -> Bool -> LearnedStrategies
learnFromPriorGame strategies [] _ = strategies
learnFromPriorGame strategies (round:priorRounds) didWin = learnFromPriorGame newStrategy priorRounds didWin where
  newStrategy = learnFromSingleMove strategies round didWin

learnFromSingleMove :: LearnedStrategies -> (GameState, Move) -> Bool -> LearnedStrategies
learnFromSingleMove strategies (state, Move mark gridIndex) didWin = newStrategy:otherStrategies where
  currentStrategy = fromMaybe (initStrategy state) $ (Strategy state) <$> (getStrategyFromGameState strategies state)
  newStrategy = updateStrategy currentStrategy gridIndex didWin
  otherStrategies = delete currentStrategy strategies

updateStrategy :: Strategy -> GridIndex -> Bool -> Strategy
updateStrategy (Strategy state currentMoves) gridIndex didWin = Strategy state (newMoves currentMoves) where
  newMoves = unflattenPossibleMoves . map updateWeighting . flattenPossibleMoves
  updateWeighting (weighting, index) = (weightingUpdateFunc weighting, index) where
    weightingUpdateFunc = weightingUpdateFunction state gridIndex index didWin currentMoves

weightingUpdateFunction :: GameState -> GridIndex -> GridIndex -> Bool -> PossibleMoves -> (MoveWeighting -> MoveWeighting)
weightingUpdateFunction state moveIndex thisIndex didWin currentMoves
  | isIllegalMove state moveIndex = id
  | moveIndex == thisIndex && didWin = (+) (countOfPunishablyPotentialMoves currentMoves)
  | moveIndex == thisIndex && not didWin = max minWeighting . (-) 8
  | moveIndex /= thisIndex && didWin = max minWeighting . (-) 1
  | moveIndex /= thisIndex && not didWin = (+) 1

countOfPunishablyPotentialMoves :: PossibleMoves -> Int
countOfPunishablyPotentialMoves = foldr (\ row total -> total + numOfPunishableInRow row) 0 where
  numOfPunishableInRow = length . filter cellIsPunishable
  cellIsPunishable weight
    | weight > minWeighting = True
    | otherwise = False

isIllegalMove (State grid _) (i, j) = case grid !! i !! j of
  Played _ -> True
  Unplayed -> False

initStrategy :: GameState -> Strategy
initStrategy (State grid winner) = Strategy (State grid winner) $ map (map initWeightings) grid where
  initWeightings (Played _) = 0
  initWeightings (Unplayed) = 25 -- arbitrarily chosen

minWeighting :: MoveWeighting
minWeighting = 1

-----------------------------
-- getStrategyFromGameState
-----------------------------

getStrategyFromGameState :: LearnedStrategies -> GameState -> Maybe PossibleMoves
getStrategyFromGameState strategies state
  | length isomorphicGameStates == 1 = case head isomorphicGameStates of
      Strategy _ moves -> Just moves
  | otherwise = Nothing where
      isomorphicGameStates = filter (\ (Strategy state' _) -> areIsomorphic state state') strategies

---------------
-- selectMove
---------------

selectMove :: PossibleMoves -> MoveWeighting -> Mark -> Maybe Move
selectMove moves weightingOffset mark = Move mark <$> (selectMove' weightingOffset $ flattenPossibleMoves moves)

type FlattenedPossibleMoves = (MoveWeighting, GridIndex)

selectMove' :: MoveWeighting -> [FlattenedPossibleMoves] -> Maybe GridIndex
selectMove' _ [] = Nothing
selectMove' k ((weight, index):xs) = if k - weight < 0 then Just index
  else selectMove' (k - weight) xs

flattenPossibleMoves :: PossibleMoves -> [FlattenedPossibleMoves]
flattenPossibleMoves x = concat $ flattenEachRow x 0 where
  flattenEachRow [] _ = []
  flattenEachRow (x:xs) i = flattenRow x i 0 : flattenEachRow xs (i + 1) where
    flattenRow [] _ _ = []
    flattenRow (x:xs) i j = (x, (i, j)) : flattenRow xs j (j + 1)

unflattenPossibleMoves :: [FlattenedPossibleMoves] -> PossibleMoves
unflattenPossibleMoves moves = map (rowFilter moves) [1,2,3] where
  rowFilter moves' i' = map (\(weighting, _) -> weighting)
                          $ filter (\ (weighting, (i,j)) -> i' == i) moves