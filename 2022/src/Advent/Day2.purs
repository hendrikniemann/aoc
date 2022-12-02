module Advent.Day2
  ( solve
  , solveExtra
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sum, traverse)
import Util (inputToRows)

data Choice = Rock | Paper | Scissors

solve :: String -> Either String String
solve = inputToRows >>> traverse score >>> map sum >>> map show

score :: String -> Either String Int
score = toCharArray >>> case _ of
  ['A', _, 'X'] -> Right 4
  ['A', _, 'Y'] -> Right 8
  ['A', _, 'Z'] -> Right 3
  ['B', _, 'X'] -> Right 1
  ['B', _, 'Y'] -> Right 5
  ['B', _, 'Z'] -> Right 9
  ['C', _, 'X'] -> Right 7
  ['C', _, 'Y'] -> Right 2
  ['C', _, 'Z'] -> Right 6
  _ -> Left "Could not parse row"

solveExtra :: String -> Either String String
solveExtra = inputToRows >>> traverse strategy >>> map sum >>> map show

-- X lose, Y draw, and Z win
strategy :: String -> Either String Int
strategy = toCharArray >>> case _ of
  ['A', _, 'X'] -> Right 3
  ['A', _, 'Y'] -> Right 4
  ['A', _, 'Z'] -> Right 8
  ['B', _, 'X'] -> Right 1
  ['B', _, 'Y'] -> Right 5
  ['B', _, 'Z'] -> Right 9
  ['C', _, 'X'] -> Right 2
  ['C', _, 'Y'] -> Right 6
  ['C', _, 'Z'] -> Right 7
  _ -> Left "Could not parse row"
