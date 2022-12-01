module Util where

import Prelude

import Data.Array (filter)
import Data.Array as Array
import Data.Either (fromRight')
import Data.Foldable (class Foldable, maximumBy, minimumBy)
import Data.Maybe (Maybe)
import Data.String (split, Pattern(..))
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Partial.Unsafe (unsafeCrashWith)


count :: forall a. (a -> Boolean) -> Array a -> Int
count p = Array.filter p >>> Array.length


unsafeSimpleRegex :: String -> Regex.Regex
unsafeSimpleRegex str =
  fromRight'
    (\_ -> unsafeCrashWith ("Failed to parse Regex \"" <> str <> "\"!"))
    (Regex.regex str noFlags)

inputToRows :: String -> Array String
inputToRows = split (Pattern "\n") >>> filter (_ /= "")


minimumWith :: forall f a b. Foldable f => Ord b => (a -> b) -> f a -> Maybe a
minimumWith g = minimumBy (\a b -> compare (g a) (g b))


maximumWith :: forall f a b. Foldable f => Ord b => (a -> b) -> f a -> Maybe a
maximumWith g = maximumBy (\a b -> compare (g a) (g b))
