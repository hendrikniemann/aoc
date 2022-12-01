module Advent.Day5
  ( solve
  , solveExtra
  )
  where

import Prelude

import Data.Array (filter, foldl, zip, (..))
import Data.Either (Either, note)
import Data.Int (fromString)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Util (inputToRows)


solve :: String -> Either String String
solve input = do
  lineDefinitions <- note "Could not parse input to lines" $ parseLineDefinitions input
  let filteredLines = filter relevantLine lineDefinitions
  let lines = filteredLines >>= definitionToLine

  let field = foldl (flip (Map.alter (maybe 1 (_ + 1) >>> pure))) Map.empty lines

  pure $ show $ Map.size $ Map.filter (_ > 1) field
    where
      relevantLine { from: Tuple x1 y1, to: Tuple x2 y2 } = x1 == x2 || y1 == y2

solveExtra :: String -> Either String String
solveExtra input = do
  lineDefinitions <- note "Could not parse input to lines" $ parseLineDefinitions input
  let lines = lineDefinitions >>= definitionToLine

  let field = foldl (flip (Map.alter (maybe 1 (_ + 1) >>> pure))) Map.empty lines

  pure $ show $ Map.size $ Map.filter (_ > 1) field


type Definition = { from :: Tuple Int Int, to :: Tuple Int Int }


parseLineDefinitions :: String -> Maybe (Array Definition)
parseLineDefinitions input = traverse parseDefinition $ inputToRows input
  where
    parseDefinition str = case split (Pattern " -> ") str of
      [pair1, pair2] -> { from: _, to: _ } <$> parseCoords pair1 <*> parseCoords pair2
      _ -> Nothing
    parseCoords str = case split (Pattern ",") str of
      [x, y] -> Tuple <$> fromString x <*> fromString y
      _ -> Nothing


type Line = Array (Tuple Int Int)


definitionToLine :: Definition -> Line
definitionToLine { from: Tuple x1 y1, to: Tuple x2 y2 }
  | x1 == x2 = y1 .. y2 <#> (Tuple x1)
  | y1 == y2 = x1 .. x2 <#> flip Tuple y1
  | otherwise = zip (x1 .. x2) (y1 .. y2)
