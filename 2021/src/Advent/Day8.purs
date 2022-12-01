module Advent.Day8
  ( solve
  , solveExtra
  )
  where

import Prelude

import Data.Array (elem, elemIndex, filter, find)
import Data.Either (Either(..), note)
import Data.Foldable (sum)
import Data.Int (fromString)
import Data.Maybe (Maybe)
import Data.Set as Set
import Data.String (Pattern(..), split, trim, length)
import Data.String.CodeUnits (charAt, fromCharArray, toCharArray)
import Data.Traversable (traverse)
import Util (count, inputToRows)


solve :: String -> Either String String
solve input = do
  lines <- traverse parseNote $ inputToRows input
  let outputs = lines >>= _.outputs
  let result = count (length >>> (_ `elem` [2, 4, 3, 7])) outputs
  pure $ show $ result


solveExtra :: String -> Either String String
solveExtra input = do
  lines <- traverse parseNote $ inputToRows input
  results <- note "Could not decode all notes" $ traverse decode lines
  pure $ show $ sum results

parseNote :: String -> Either String { signals :: Array String, outputs :: Array String }
parseNote line =
  case split (Pattern " | ") line of
    [signalPatternString, outputValueString] -> do
      let signals = trim <$> split (Pattern " ") signalPatternString
      let outputs = trim <$> split (Pattern " ") outputValueString
      pure $ { signals, outputs }

    _ ->
      Left "Could not parse line to signals and output"


decode :: { signals :: Array String, outputs :: Array String } -> Maybe Int
decode arrays = do
  let signals = map toCharSet arrays.signals
  let eight = toCharSet "abcdefg"
  -- 1 is the only signal with two panels on
  one <- find (ofSize 2) signals
  four <- find (ofSize 4) signals
  seven <- find (ofSize 3) signals
  -- F is in all numbers except two: F is in 9 numbers
  let occurrences = toCharArray "abcdefg" <#> \letter -> count (Set.member letter) signals
  f <- elemIndex 9 occurrences >>= \index -> charAt index "abcdefg"
  -- E is in 4 numbers
  e <- elemIndex 4 occurrences >>= \index -> charAt index "abcdefg"

  two <- find (not <<< Set.member f) signals
  -- If we know F and we know that 1 contains C and F, we know C
  c <- Set.toUnfoldable $ Set.delete f one
  -- 5 and 6 both don't contain C
  let fiveAndSix = filter (not <<< Set.member c) signals
  -- 5 doesn't contain E
  five <- find (not <<< Set.member e) fiveAndSix
  -- But 6 does
  six <- find (Set.member e) fiveAndSix
  -- 6, 9 and zero have 6 panels
  let sixAndNineAndZero = filter (ofSize 6) signals
  -- 9 does not have the E panel
  nine <- find (not <<< Set.member e) sixAndNineAndZero
  -- and 0 is neither 9 nor 6, duh
  zero <- find (\n -> (n /= six) && (n /= nine)) sixAndNineAndZero

  let charArray = arrays.outputs <#> toCharSet <#> case _ of
        digit | digit == one -> '1'
        digit | digit == two -> '2'
        digit | digit == four -> '4'
        digit | digit == five -> '5'
        digit | digit == six -> '6'
        digit | digit == seven -> '7'
        digit | digit == eight -> '8'
        digit | digit == nine -> '9'
        digit | digit == zero -> '0'
        _ -> '3'

  fromString $ fromCharArray charArray
    where
      ofSize n set = Set.size set == n
      toCharSet = toCharArray >>> Set.fromFoldable
