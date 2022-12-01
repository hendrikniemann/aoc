module Advent.Day7
  ( solve
  , solveExtra
  )
  where

import Prelude

import Data.Array ((..), mapWithIndex)
import Data.Either (Either, note)
import Data.Foldable (minimumBy, sum)
import Data.Int (fromString)
import Data.Ord (abs)
import Data.String (Pattern(..), split, trim)
import Data.Traversable (maximum, traverse)
import Data.Tuple (Tuple(..), snd)


solve :: String -> Either String String
solve input = do
  crabPositions <- note "Could not parse input" $
    traverse fromString $ trim <$> split (Pattern ",") input

  maxHorizontalPosition <- note "No positions supplied" $ maximum crabPositions

  let costs = 0..maxHorizontalPosition <#> \depth -> sum $ map (fuelCost depth) crabPositions

  let compareCosts a b = compare (snd a) (snd b)
  Tuple _ cost <- note "minimum not found" $ minimumBy compareCosts $ mapWithIndex Tuple costs

  pure $ show cost


solveExtra :: String -> Either String String
solveExtra input = do
  crabPositions <- note "Could not parse input" $
    traverse fromString $ trim <$> split (Pattern ",") input

  maxHorizontalPosition <- note "No positions supplied" $ maximum crabPositions

  let costs = 0..maxHorizontalPosition <#> \depth ->
        sum $ map (fuelCost depth >>> sumNums) crabPositions

  let compareCosts a b = compare (snd a) (snd b)
  Tuple _ cost <- note "minimum not found" $ minimumBy compareCosts $ mapWithIndex Tuple costs

  pure $ show cost

fuelCost :: Int -> Int -> Int
fuelCost a b = abs (a - b)


-- | Calculate the sum of all numbers from 0 to n
sumNums :: Int -> Int
sumNums n = (n + 1) * n / 2
