module Advent.Day6 where

import Prelude

import Data.Array as Array
import Data.Either (Either)
import Data.Enum (enumFromTo)
import Data.Foldable (sum)
import Data.List (foldl)
import Data.Set as Set
import Data.String (Pattern(..), split, trim)
import Data.String.CodeUnits (toCharArray)


solve :: String -> Either String String
solve =
  split (Pattern "\n\n") >>>
  map trim >>>
  map countAnswered2 >>>
  sum >>>
  show >>>
  pure


countAnswered1 :: String -> Int
countAnswered1 str = Set.size $ Set.fromFoldable $ Array.filter (between 'a' 'z') $ toCharArray str


countAnswered2 :: String -> Int
countAnswered2 str =
  Set.size $
  foldl Set.intersection setAll $
  map Set.fromFoldable $
  map toCharArray $
  split (Pattern "\n") str


setAll :: Set.Set Char
setAll = (Set.fromFoldable :: Array Char -> Set.Set Char) $ enumFromTo 'a' 'z'
