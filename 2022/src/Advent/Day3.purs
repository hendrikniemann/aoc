module Advent.Day3
  ( solve
  , solveExtra
  )
  where


import Prelude

import Data.Array (drop, head, intersect, length, mapMaybe, take)
import Data.Either (Either(..), note)
import Data.Foldable (foldl, sum)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String.CodeUnits (indexOf, singleton, toCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), uncurry)
import Data.Unfoldable (unfoldr)
import Util (inputToRows)


solve :: String -> Either String String
solve input = do
  let rucksacks = inputToRows input
  let priorities = mapMaybe (splitCompartments >>> intersectingElement >=> priority) rucksacks
  Right $ show $ sum $ priorities


splitCompartments :: String -> Tuple (Array Char) (Array Char)
splitCompartments input =
  let array = toCharArray input
      len = length array
      firstCompartment = take (len / 2) array
      secondCompartment = drop (len / 2) array
  in Tuple firstCompartment secondCompartment


intersectingElement :: Tuple (Array Char) (Array Char) -> Maybe Char
intersectingElement = uncurry intersect >>> head


allChars :: String
allChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"


priority :: Char -> Maybe Int
priority char =
  add 1 <$>
    indexOf (Pattern $ singleton char) allChars


solveExtra :: String -> Either String String
solveExtra input = do
  let rucksacks = inputToRows input
  let groups = chunks 3 rucksacks
  priorities <-
    traverse ((head >=> priority) >>> note "not all groups intersect") $
    map (foldl intersect $ toCharArray allChars) $
    map (map toCharArray) groups
  Right $ show $ sum $ priorities


chunks :: forall a. Int -> Array a -> Array (Array a)
chunks size = unfoldr case _ of
  [] -> Nothing
  arr -> Just $ Tuple (take size arr) (drop size arr)
