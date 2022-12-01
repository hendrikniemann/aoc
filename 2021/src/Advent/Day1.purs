module Advent.Day1
  ( solve
  , solveExtra
  )
  where

import Prelude

import Data.Array (filter, foldl, length, take, (:))
import Data.Array.Partial (tail)
import Data.Either (Either, note)
import Data.Int (fromString)
import Data.String (Pattern(..), split)
import Data.Traversable (sum, traverse)
import Data.Tuple (Tuple(..), snd)
import Partial.Unsafe (unsafePartial)


solve :: String -> Either String String
solve =
  split (Pattern "\n") >>>
  filter (_ /= "") >>>
  (traverse fromString >>> note "Not all input lines are integers!") >>>
  map countDepthIncrease >>>
  map show

solveExtra :: String -> Either String String
solveExtra =
  split (Pattern "\n") >>>
  filter (_ /= "") >>>
  (traverse fromString >>> note "Not all input lines are integers!") >>>
  map countDepthIncreaseExtra >>>
  map show


countDepthIncrease :: Array Int -> Int
countDepthIncrease array = snd $ foldl countIncreases (Tuple 100000 0) array


countIncreases :: Tuple Int Int -> Int -> Tuple Int Int
countIncreases (Tuple prev count) next
  | prev < next = Tuple next (count + 1)
  | otherwise = Tuple next count


countDepthIncreaseExtra :: Array Int -> Int
countDepthIncreaseExtra array =
  snd $
    foldl countIncreases (Tuple 100000 0) $
    map sum $
    tripelsInArray array


tripelsInArray :: Array Int -> Array (Array Int)
tripelsInArray array
  | length array < 3 = []
  | otherwise   = take 3 array : tripelsInArray (unsafePartial tail array)
