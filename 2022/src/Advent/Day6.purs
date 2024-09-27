module Advent.Day6
  ( solve
  , solveExtra
  )
  where


import Prelude

import Data.Either (Either, note)
import Data.Foldable (class Foldable, length)
import Data.List (List, fromFoldable, (:), take)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String.CodeUnits (toCharArray)


solve :: String -> Either String String
solve input = do
  let chars = fromFoldable $ toCharArray input
  note "could not find non-repeating sequence" $ show <$> findNonRepeating chars


areAllUnique :: forall f. Foldable f => f Char -> Boolean
areAllUnique list = length list == (Set.size $ Set.fromFoldable list)


findNonRepeating :: List Char -> Maybe Int
findNonRepeating (a:b:c:d:xs) | areAllUnique [a, b, c, d] = Just 4
                              | otherwise = add 1 <$> findNonRepeating (b:c:d:xs)
findNonRepeating _ = Nothing


solveExtra :: String -> Either String String
solveExtra input = do
  let chars = fromFoldable $ toCharArray input
  note "could not find non-repeating sequence" $ show <$> findNonRepeating' 14 chars


findNonRepeating' :: Int -> List Char -> Maybe Int
findNonRepeating' n l@(_:xs) | length l >= n && areAllUnique (take n l) = Just n
                             | otherwise = add 1 <$> findNonRepeating' n xs
findNonRepeating' _ _ = Nothing