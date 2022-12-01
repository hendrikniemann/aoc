module Advent.Day9
  ( solve
  , solveExtra
  )
  where

import Prelude

import Data.Array (all, filter, mapMaybe)
import Data.Either (Either(..), note)
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.Maybe (maybe)
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (traverse)
import Debug (spy)
import Matrix as Matrix
import Util (inputToRows)


solve :: String -> Either String String
solve input = do
  heightMap <- note "Could not turn input into matrix, line lengths might not align." do
    intArray <- traverse (traverse fromString) $ spy "intArray" $ split (Pattern "") <$> inputToRows input
    Matrix.fromArray $ intArray

  let lowpoints = findLowpoints heightMap

  pure $ show $ sum $ map (_.value >>> (_ + 1)) $ lowpoints


solveExtra :: String -> Either String String
solveExtra input = do
  heightMap <- note "Could not turn input into matrix, line lengths might not align." do
    intArray <- traverse (traverse fromString) $ spy "intArray" $ split (Pattern "") <$> inputToRows input
    Matrix.fromArray $ intArray

  let lowpoints = findLowpoints heightMap


  Left "Unsolved"

findLowpoints :: Matrix.Matrix Int -> Array { value :: Int, x :: Int, y :: Int }
findLowpoints heightMap = heightMap # Matrix.toIndexedArray # filter \({ value, x, y }) ->
  let
    adjected =
      [ Matrix.get (x - 1) y heightMap
      , Matrix.get (x + 1) y heightMap
      , Matrix.get x (y - 1) heightMap
      , Matrix.get x (y + 1) heightMap
      ]
  in
    all (maybe true (_ > value)) adjected
