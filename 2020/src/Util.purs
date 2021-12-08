module Util where

import Prelude

import Data.Array as Array
import Data.Either (fromRight')
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
