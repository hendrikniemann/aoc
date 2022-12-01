module Advent.Day10
  ( consume
  , solve
  , solveExtra
  )
  where

import Prelude

import Data.Array (foldM, index, length, mapMaybe, sort)
import Data.BigInt as BigInt
import Data.Either (Either(..), either, note)
import Data.Foldable (sum)
import Data.List (List(..), (:))
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import Util (inputToRows)


solve :: String -> Either String String
solve input = do
  let rows = toCharArray <$> inputToRows input
  let illegalCharacters = mapMaybe (consume >>> either Just (const Nothing)) rows
  let result = sum $ mapMaybe (flip Map.lookup points) illegalCharacters
  pure $ show result


solveExtra :: String -> Either String String
solveExtra input = do
  let rows = toCharArray <$> inputToRows input
  let incompletions = mapMaybe (consume >>> either (const Nothing) Just) rows
  let scores =
        List.foldl (\acc n -> acc * (BigInt.fromInt 5) + (BigInt.fromInt n)) (BigInt.fromInt 0) <$>
        List.mapMaybe (_ `Map.lookup` pointsExtra) <$>
        incompletions
  note "Could not find median" $ map show $ median scores


pairs :: Map.Map Char Char
pairs = Map.fromFoldable [Tuple '(' ')', Tuple '[' ']', Tuple '<' '>', Tuple '{' '}']


points :: Map.Map Char Int
points = Map.fromFoldable [Tuple ')' 3, Tuple ']' 57, Tuple '}' 1197, Tuple '>' 25137]


pointsExtra :: Map.Map Char Int
pointsExtra = Map.fromFoldable [Tuple '(' 1, Tuple '[' 2, Tuple '{' 3, Tuple '<' 4]


isOpening :: Char -> Boolean
isOpening char = Map.member char pairs


closes :: Char -> Char -> Boolean
closes a b = Map.lookup b pairs == Just a


consume :: Array Char -> Either Char (List Char)
consume = foldM reducer Nil
  where
    reducer :: (List Char) -> Char -> Either Char (List Char)
    reducer Nil y
      | isOpening y = Right (y:Nil)
      | otherwise = Left 'e'
    reducer (x:xs) y
      | isOpening y = Right (y:x:xs)
      | y `closes` x = Right xs
      | otherwise = Left y


median :: forall a. Ord a => Array a -> Maybe a
median arr = index (sort arr) (length arr / 2)
