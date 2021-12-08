module Advent.Day1 where

import Prelude

import Control.Apply (lift2)
import Data.Array (filter)
import Data.Either (Either, note)
import Data.Int (fromString)
import Data.List (find)
import Data.Maybe (Maybe)
import Data.Set (fromFoldable, member)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), uncurry)


solve :: String -> Either String String
solve =
  split (Pattern "\n") >>>
  filter (_ /= "") >>>
  (traverse fromString >>> note "Not all input lines are integers!") >=>
  (findProduct >>> note "Could not find a solution to the problem") >=>
  (show >>> pure)


findProduct :: Array Int -> Maybe Int
findProduct array =
  let
    tuples = lift2 Tuple array array
    set = fromFoldable array
    sumIs2020 x y = member (2020 - x - y) set
    makeResult (Tuple x y) = x * y * (2020 - x - y)
  in
    makeResult <$> find (uncurry sumIs2020) tuples
