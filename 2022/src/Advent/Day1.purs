module Advent.Day1
  ( solve
  , solveExtra
  )
  where

import Prelude

import Data.Array (reverse, sort, take)
import Data.Either (Either, note)
import Data.Int (fromString)
import Data.Maybe (Maybe)
import Data.String (Pattern(..), split)
import Data.Traversable (maximum, sequence, sum, traverse)
import Util (inputToRows)


solve :: String -> Either String String
solve =
  split (Pattern "\n\n") >>>
  map inputToRows >>>
  map (sumStringArray >>> note "Not all input lines are integers!") >>>
  sequence >>>
  map maximum >>>
  map show


sumStringArray :: Array String -> Maybe Int
sumStringArray = traverse fromString >>> map sum


solveExtra :: String -> Either String String
solveExtra =
  split (Pattern "\n\n") >>>
  map inputToRows >>>
  map (sumStringArray >>> note "Not all input lines are integers!") >>>
  sequence >>>
  map (sort >>> reverse >>> take 3 >>> sum) >>>
  map show
