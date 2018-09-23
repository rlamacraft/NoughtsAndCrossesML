module Printing (printAside) where

import Data.List (maximumBy)

-- This module is a set of utilities for printing different classes

printAside :: Show a => Show b => a -> b -> String
printAside a b = concat $ map concatLines $ zip leftLines rightLines where
  concatLines :: (String, String) -> String
  concatLines (leftLine, rightLine) = leftLine ++ (tab leftLine) ++ rightLine ++ "\n"  --TODO: Spacing should ensure that all right lines are right of all left lines
  leftLines = lines $ show a
  rightLines = lines $ show b
  tab :: String -> String
  tab leftLine = replicate (longestLineLength - length leftLine + 3) ' '
  longestLineLength = maximumBy compare $ map length leftLines

newtype TestType = TT Int

instance Show TestType where
  show (TT _) = "this is a first line \nthis is a second line \nthis is a third line"
