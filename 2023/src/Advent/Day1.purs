module Advent.Day1 where

import Prelude

import Data.Array (filter, fold, head, intercalate, last)
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..), note)
import Data.Int (fromString)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Set (Set, fromFoldable, member)
import Data.String.CodeUnits (fromCharArray, singleton, toCharArray)
import Data.String.Regex (match)
import Data.Traversable (sequence, sum, traverse)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Util (inputToRows, unsafeSimpleRegex)

solve :: String -> Either String String
solve = inputToRows >>> traverse getNumberOfLine >>> map (sum) >>> map show

getNumberOfLine :: String -> Either String Int
getNumberOfLine line = do
  let digits = filter (flip member numbers) $ toCharArray line
  num <- note "No numbers found in line" $ sequence [head digits, last digits]
  note "Coult not parse int" $ fromString $ fromCharArray num


numbers :: Set Char
numbers = fromFoldable $ toCharArray "0123456789"

solveExtra :: String -> Either String String
solveExtra _ = inputToRows >>> traverse getNumberOfLineExtra >>> map (sum) >>> map show


getNumberOfLineExtra :: String -> Either String Int
getNumberOfLineExtra line = do
  let digits = filter (flip member numbers) $ toCharArray line
  let firstDigit =  line
  num <- note "No numbers found in line" $ sequence [head digits, last digits]
  note "Coult not parse int" $ fromString $ fromCharArray num
  where
    regexForward = unsafeSimpleRegex "([0-9]|zero|one|two|three|four|five|six|seven|eight|nine)"
    regexBackward = unsafeSimpleRegex "([0-9]|orez|enin|thgie|neves|xis|evif|ruof|eerht|owt|eno)"

    findFirstDigit :: String -> Maybe Int
    findFirstDigit = do
      res <- match regexForward line
      first <- NEA.head res
      Map.lookup first assignments

    assignments :: Map.Map String Int
    assignments = Map.fromFoldable
      [ Tuple "1" 1
      , Tuple "one" 1
      , Tuple "eno" 1
      , Tuple "2" 2
      , Tuple "two" 2
      , Tuple "owt" 2
      , Tuple "3" 3
      , Tuple "three" 3
      , Tuple "eerht" 3
      , Tuple "4" 4
      , Tuple "four" 4
      , Tuple "ruof" 4
      , Tuple "5" 5
      , Tuple "five" 5
      , Tuple "evif" 5
      , Tuple "6" 6
      , Tuple "six" 6
      , Tuple "xis" 6
      , Tuple "7" 7
      , Tuple "seven" 7
      , Tuple "neves" 7
      , Tuple "8" 8
      , Tuple "eight" 8
      , Tuple "thgie" 8
      , Tuple "9" 9
      , Tuple "nine" 9
      , Tuple "enin" 9
      , Tuple "0" 0
      , Tuple "zero" 0
      , Tuple "orez" 0
      ]


forward = unsafeSimpleRegex "([0-9]|zero|one|two|three|four|five|six|seven|eight|nine)"
