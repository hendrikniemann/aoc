module Advent.Day6
  ( solve
  , solveExtra
  )
  where

import Prelude

import Data.Array (groupAll)
import Data.Array.NonEmpty as NEA
import Data.Int (fromString)
import Data.BigInt (BigInt, fromInt)
import Data.Either (Either, note)
import Data.Function (applyN)
import Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.String (Pattern(..), split, trim)
import Data.Traversable (sum, traverse)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)


solve :: String -> Either String String
solve input = do
  initialFish <- note "Could not parse input" $
    traverse fromString $ trim <$> split (Pattern ",") input

  let groupSettings = groupAll initialFish <#> \grp -> Tuple (NEA.head grp) (NEA.length grp)
  let groupMap = Map.fromFoldable groupSettings
  let startValue timer = fromMaybe 0 $ Map.lookup timer groupMap
  let startArray = NEA.range 0 8 <#> startValue <#> fromInt

  pure $ show $ sum $ applyN progress 80 startArray


solveExtra :: String -> Either String String
solveExtra input = do
  initialFish <- note "Could not parse input" $
    traverse fromString $ trim <$> split (Pattern ",") input

  let groupSettings = groupAll initialFish <#> \grp -> Tuple (NEA.head grp) (NEA.length grp)
  let groupMap = Map.fromFoldable groupSettings
  let startValue timer = fromMaybe 0 $ Map.lookup timer groupMap
  let startArray = NEA.range 0 8 <#> startValue <#> fromInt

  pure $ show $ sum $ applyN progress 256 startArray


progress :: NEA.NonEmptyArray BigInt -> NEA.NonEmptyArray BigInt
progress generation =
  let
    reproducing = NEA.head generation
    rest = NEA.tail generation
  in
    unsafePartial fromJust $ NEA.modifyAt 6 (_ + reproducing) $ NEA.snoc' rest reproducing

