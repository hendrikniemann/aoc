module Advent.Day13
  ( parsePoint
  , solve
  , solveExtra
  )
  where


import Prelude

import Data.Array (head)
import Data.Array.NonEmpty (uncons)
import Data.Either (Either(..), note)
import Data.Foldable (foldl, foldM)
import Data.Int (fromString)
import Data.List (intercalate)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (fromCharArray)
import Data.String.Regex as R
import Data.Traversable (maximumBy, traverse)
import Data.Tuple (Tuple(..))
import Matrix as M
import Util (inputToRows, unsafeSimpleRegex)


solve :: String -> Either String String
solve input = do
  Tuple points folds <-
    case split (Pattern"\n\n") input of
      [p, f] ->
        Tuple <$>
          traverse parsePoint (inputToRows p) <*>
          traverse parseFold (inputToRows f)
      _ ->
        Left "Unable to split up input into points and folds"
  firstFold <- note "No folds were found" $ head folds
  pure $ show $ Set.size $ foldPaper (Set.fromFoldable points) firstFold


solveExtra :: String -> Either String String
solveExtra input = do
  Tuple points folds <-
    case split (Pattern"\n\n") input of
      [p, f] ->
        Tuple <$>
          traverse parsePoint (inputToRows p) <*>
          traverse parseFold (inputToRows f)
      _ ->
        Left "Unable to split up input into points and folds"
  let remainingPoints = foldl foldPaper (Set.fromFoldable points) folds
  field <- note "Could not find map dimensions" $ M.repeat <$>
    (maximumBy (\a b -> compare a.x b.x) remainingPoints <#> _.x <#> (_ + 1)) <*>
    (maximumBy (\a b -> compare a.y b.y) remainingPoints <#> _.y <#> (_ + 1)) <*>
    pure '.'
  field' <- note "Failed to draw map" $
    foldM (\fld { x, y } -> M.set x y '#' fld) field remainingPoints
  pure $ ("\n" <> _) $ intercalate "\n" $ fromCharArray <$> M.rows field'

type Point = { x :: Int, y :: Int }


data Fold = X Int | Y Int


parsePoint :: String -> Either String Point
parsePoint s =
  note ("Could not parse string \"" <> s <> "\" to point.") $
    case split (Pattern ",") s of
      [x, y] -> { x: _, y: _ } <$> fromString x <*> fromString y
      _ -> Nothing


parseFold :: String -> Either String Fold
parseFold s =
  note ("Could not parse string \"" <> s <> "\" to fold.") $
    case R.match foldRegex s <#> uncons of
      Just { tail: [Just "x", Just line] } -> X <$> fromString line
      Just { tail: [Just "y", Just line] } -> Y <$> fromString line
      _ -> Nothing
  where
    foldRegex = unsafeSimpleRegex """fold along ([xy])=(\d+)"""


foldPaper :: Set.Set Point -> Fold -> Set.Set Point
foldPaper points (X line) = points `flip Set.map` case _ of
  { x, y } | x > line -> { x: line - (x - line), y }
  point -> point
foldPaper points (Y line) = points `flip Set.map` case _ of
  { x, y } | y > line -> { y: line - (y - line), x }
  point -> point
