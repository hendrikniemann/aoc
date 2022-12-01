module Advent.Day14
  ( solve
  , solveExtra
  )
  where


import Prelude

import Control.Apply (lift2)
import Data.Either (Either(..), note)
import Data.Foldable (foldl, maximum, minimum)
import Data.Function (applyN)
import Data.List ((:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), trim, split)
import Data.String.CodeUnits (toCharArray)
import Data.String.Unsafe (charAt)
import Data.Traversable (class Foldable, traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Util (inputToRows, maximumWith, minimumWith)


solve :: String -> Either String String
solve input = do
  Tuple polymer rules <-
    case split (Pattern"\n\n") input of
      [p, r] ->
        Tuple <$>
          pure (trim p) <*>
          traverse parseInsertionRule (inputToRows r)
      _ ->
        Left "Unable to split up input into polymer and rules"
  let ruleMap = Map.fromFoldable rules
  let polymerList = List.fromFoldable $ toCharArray polymer
  let grownPolymer = applyN (insertPolymer ruleMap) 10 polymerList
  let occurrences = countOccurrences grownPolymer
  let min = minimum $ snd <$> (Map.toUnfoldable occurrences :: Array (Tuple Char Int))
  let max = maximum $ snd <$> (Map.toUnfoldable occurrences :: Array (Tuple Char Int))
  note "could not find min or max" $ show <$> lift2 (-) max min


solveExtra :: String -> Either String String
solveExtra input = do
  Tuple polymer rules <-
    case split (Pattern"\n\n") input of
      [p, r] ->
        Tuple <$>
          pure (trim p) <*>
          traverse parseInsertionRule (inputToRows r)
      _ ->
        Left "Unable to split up input into polymer and rules"
  let ruleMap = Map.fromFoldable rules
  let polymerList = List.fromFoldable $ toCharArray polymer
  let grownPolymer = applyN (insertPolymer ruleMap) 40 polymerList
  let occurrences = countOccurrences grownPolymer
  let min = minimum $ snd <$> (Map.toUnfoldable occurrences :: Array (Tuple Char Int))
  let max = maximum $ snd <$> (Map.toUnfoldable occurrences :: Array (Tuple Char Int))
  note "could not find min or max" $ show <$> lift2 (-) max min

parseInsertionRule :: String -> Either String (Tuple (Tuple Char Char) Char)
parseInsertionRule str = case split (Pattern " -> ") str of
  [pair, single] -> pure $ Tuple (Tuple (charAt 0 pair) (charAt 1 pair)) (charAt 0 single)
  _ -> Left ("Could not parse string \"" <> str <> "\" to insertion rule")

insertPolymer :: Map.Map (Tuple Char Char) Char -> List.List Char -> List.List Char
insertPolymer rules list = go list List.Nil
  where
    go (x:y:xs) acc =
      case Map.lookup (Tuple x y) rules of
        Just z -> go (y:xs) (z:x:acc)
        Nothing -> go (y:xs) (x:acc)
    go (x:List.Nil) acc = List.reverse (x:acc)
    go List.Nil acc = List.reverse acc

countOccurrences :: forall f a. Ord a => Foldable f => f a -> Map.Map a Int
countOccurrences = foldl (flip $ Map.alter (fromMaybe 0 >>> (_ + 1) >>> pure)) Map.empty
