module Advent.Day2
  ( solve
  , solveExtra
  )
  where

import Prelude

import Data.Array (filter, foldl)
import Data.Either (Either, note)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested (T3, (/\))


solve :: String -> Either String String
solve =
  split (Pattern "\n") >>>
  filter (_ /= "") >>>
  (traverse instructionFromString >>> note "Not all input lines are instructions!") >>>
  map reducePosition >>>
  map (uncurry (*)) >>>
  map show

solveExtra :: String -> Either String String
solveExtra =
  split (Pattern "\n") >>>
  filter (_ /= "") >>>
  (traverse instructionFromString >>> note "Not all input lines are instructions!") >>>
  map reducePositionExtra >>>
  map (\(x /\ y /\ _) -> x * y) >>>
  map show

data Instruction = Down Int | Up Int | Forward Int

instructionFromString :: String -> Maybe Instruction
instructionFromString str = case split (Pattern " ") str of
  ["down", intStr] -> Down <$> fromString intStr
  ["up", intStr] -> Up <$> fromString intStr
  ["forward", intStr] -> Forward <$> fromString intStr
  _ -> Nothing


reducePosition :: Array Instruction -> Tuple Int Int
reducePosition = foldl reducer (Tuple 0 0)
  where
    reducer (Tuple x y) (Down amount) = Tuple x (y + amount)
    reducer (Tuple x y) (Up amount) = Tuple x (y - amount)
    reducer (Tuple x y) (Forward amount) = Tuple (x + amount) y

reducePositionExtra :: Array Instruction -> T3 Int Int Int
reducePositionExtra = foldl reducer (0 /\ 0 /\ 0)
  where
    reducer (x /\ y /\ aim) (Down amount) = x /\ y /\ (aim + amount)
    reducer (x /\ y /\ aim) (Up amount) = x /\ y /\ (aim - amount)
    reducer (x /\ y /\ aim) (Forward amount) = (x + amount) /\ (y + aim * amount) /\ aim
