module Advent.Day3 where

import Prelude

import Data.Array (zipWith)
import Data.Array as Array
import Data.Either (Either)
import Data.Maybe (fromJust)
import Data.String (Pattern(..), length, split)
import Data.String.CodeUnits (charAt)
import Partial.Unsafe (unsafePartialBecause)
import Util (count)

solve :: String -> Either String String
solve =
  split (Pattern "\n") >>>
  Array.filter (_ /= "") >>>
  everyX 2 >>>
  countTrees 1 >>>
  show >>>
  pure


everyX :: forall a. Int -> Array a -> Array a
everyX n xs =
  let
    index = Array.filter ((_ `mod` n) >>> (_ == 0)) $ Array.range 0 (Array.length xs)
  in
    Array.mapMaybe (Array.index xs) index

countTrees :: Int -> Array String -> Int
countTrees slope rows =
  let
    points = Array.range 0 (Array.length rows) <#> \n -> n * slope
  in
    count (_ == '#') $ zipWith getLetter rows points


getLetter :: String -> Int -> Char
getLetter row num =
  let
    index = num `mod` (length row)
  in
    unsafePartialBecause "bound is enforced by mod" $ fromJust $ charAt index row

