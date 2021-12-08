module Advent.Day2 where

import Prelude

import Data.Array (filter)
import Data.Array.NonEmpty (toNonEmpty)
import Data.Either (Either(..), note)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.NonEmpty ((:|))
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (charAt, toCharArray)
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Unsafe (char)
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafePartialBecause)
import Util (count)

newtype Policy = Policy { letter :: Char, min :: Int, max :: Int }

instance showPolicy :: Show Policy where
  show (Policy p) =
    "Policy " <> show p

derive instance newtypePolicy :: Newtype Policy _


solve :: String -> Either String String
solve =
  split (Pattern "\n") >>>
  filter (_ /= "") >>>
  traverse parsePolicy >>>
  map (count passwordIsValid2) >>>
  map show


passwordIsValid2 :: Tuple Policy String -> Boolean
passwordIsValid2 (Policy { letter, min, max } /\ password) =
  booleanXor
    (charAt (min - 1) password == (Just letter))
    (charAt (max - 1) password == (Just letter))


booleanXor :: Boolean -> Boolean -> Boolean
booleanXor a b = (a || b) && (not a || not b)


passwordIsValid :: Tuple Policy String -> Boolean
passwordIsValid (Policy { letter, min, max } /\ password) =
  (\x -> (x >= min) && (x <= max)) $ count (_ == letter) $ toCharArray password



parsePolicy :: String -> Either String (Tuple Policy String)
parsePolicy input = do
  regexp <- reg
  matches <- note "Line could not be matched!" $ match regexp input
  case toNonEmpty matches of
    _ :| [(Just m1), (Just m2), (Just l), (Just password)] ->
      let
        min = unsafePartialBecause "Regex ensures int" $ fromJust $ fromString m1
        max = unsafePartialBecause "Regex ensures int" $ fromJust $ fromString m2
        letter = char l
      in
        pure $ Policy { min, max, letter } /\ password
    _ -> Left "Unexpected amount of matches"

  where
    reg = regex "^(\\d+)-(\\d+) ([a-z]): ([a-z]+)$" noFlags