module Advent.Day5
  ( moveFromTo
  , solve
  , solveExtra
  )
  where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (toArray)
import Data.Either (Either(..), note)
import Data.Foldable (foldM, foldMap)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int as Int
import Data.List (List(..), (:))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String.CodeUnits (singleton)
import Data.String.Regex (match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (for)
import Util (inputToRows)

type Crates = Map Int (List Char)


-- Turns out input is difficult to parse today, reading this in myself
initialCrates :: Crates
initialCrates = foldlWithIndex (\index m stack -> Map.insert (index + 1) stack m) Map.empty
  [ 'W':'L':'S':Nil
  , 'Q':'N':'T':'J':Nil
  , 'J':'F':'H':'C':'S':Nil
  , 'B':'G':'N':'W':'M':'R':'T':Nil
  , 'B':'Q':'H':'D':'S':'L':'R':'T':Nil
  , 'L':'R':'H':'F':'V':'B':'J':'M':Nil
  , 'M':'J':'N':'R':'W':'D':Nil
  , 'J':'D':'N':'H':'F':'T':'Z':'B':Nil
  , 'T':'F':'B':'N':'Q':'L':'H':Nil
  ]


solve :: String -> Either String String
solve input = do
  let rows = Array.drop 9 $ inputToRows input
  let reg = unsafeRegex """^move (\d+) from (\d+) to (\d)$""" noFlags
  parsed <- rows `for` \row -> case toArray <$> match reg row of
    Just [_, nStr, fromStr, toStr] -> note "failed to parse ints" ado
      n <- Int.fromString =<< nStr
      from <- Int.fromString =<< fromStr
      to <- Int.fromString =<< toStr
      in { n, from, to }
    _ -> Left "Could not parse rows"
  finalCrates <- foldM (\crates { n, from, to } -> moveFromTo n from to crates) initialCrates parsed #
    note "Error when folding final result"
  pure $ foldMap (List.head >>> maybe "_" singleton) finalCrates


moveFromTo :: Int -> Int -> Int -> Crates -> Maybe Crates
moveFromTo n from to crates = do
  stack <- List.reverse <$> List.take n <$> Map.lookup from crates
  pure $
    Map.update ((stack <> _) >>> pure) to $
    Map.update (List.drop n >>> pure) from crates


solveExtra :: String -> Either String String
solveExtra input = do
  let rows = Array.drop 9 $ inputToRows input
  let reg = unsafeRegex """^move (\d+) from (\d+) to (\d)$""" noFlags
  parsed <- rows `for` \row -> case toArray <$> match reg row of
    Just [_, nStr, fromStr, toStr] -> note "failed to parse ints" ado
      n <- Int.fromString =<< nStr
      from <- Int.fromString =<< fromStr
      to <- Int.fromString =<< toStr
      in { n, from, to }
    _ -> Left "Could not parse rows"
  finalCrates <- foldM (\crates { n, from, to } -> moveFromTo9001 n from to crates) initialCrates parsed #
    note "Error when folding final result"
  pure $ foldMap (List.head >>> maybe "_" singleton) finalCrates


moveFromTo9001 :: Int -> Int -> Int -> Crates -> Maybe Crates
moveFromTo9001 n from to crates = do
  stack <- List.take n <$> Map.lookup from crates
  pure $
    Map.update ((stack <> _) >>> pure) to $
    Map.update (List.drop n >>> pure) from crates
