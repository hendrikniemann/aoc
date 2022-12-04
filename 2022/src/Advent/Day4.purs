module Advent.Day4
  ( solve
  , solveExtra
  )
  where


import Prelude

import Data.Either (Either(..), note)
import Data.Int as Int
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Util (count, inputToRows)


type Range = Tuple Int Int


solve :: String -> Either String String
solve input = do
  let rows = inputToRows input
  rangePairs <- traverse rowToRangedPairs rows
  pure $ show $ count fullyOverlaps rangePairs


rowToRangedPairs ∷ String → Either String (Tuple Range Range)
rowToRangedPairs row =
  case split (Pattern ",") row of
        [left, right] -> Tuple <$> rangeToTuple left <*> rangeToTuple right
        _ -> Left "Unable to split rows into pairs"


fullyOverlaps :: Tuple Range Range -> Boolean
fullyOverlaps (Tuple (Tuple ls le) (Tuple rs re)) = ls >= rs && le <= re || ls <= rs && le >= re


partlyOverlaps :: Tuple Range Range -> Boolean
partlyOverlaps (Tuple (Tuple ls le) (Tuple rs re)) = ls <= re && rs <= le


rangeToTuple :: String -> Either String Range
rangeToTuple range = case split (Pattern "-") range of
  [start, end] ->
    note "Unable to parse range components into int." $
      Tuple <$> Int.fromString start <*> Int.fromString end
  _ -> Left "Unable to parse range, no \"-\" found."


solveExtra :: String -> Either String String
solveExtra input = do
  let rows = inputToRows input
  rangePairs <- traverse rowToRangedPairs rows
  pure $ show $ count partlyOverlaps rangePairs
