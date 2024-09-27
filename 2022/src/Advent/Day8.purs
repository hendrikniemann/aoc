module Advent.Day8
  ( solve
  , solveExtra
  )
  where


import Prelude

import Data.Array (all, any, drop, fromFoldable, reverse, take, uncons)
import Data.Either (Either(..), note)
import Data.Foldable (product)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split)
import Data.Traversable (maximum, sequence, traverse)
import Matrix as M
import Util (count, inputToRows)


solve :: String -> Either String String
solve input = do
  grid <- note "failed to build grid" $ sequence $
    traverse Int.fromString <$> split (Pattern "") <$> inputToRows input
  matrix <- note "failed to create matrix" $ M.fromArray grid
  let isVisible x y height = fromMaybe false $ any (all (_ < height)) <$> sequence
        [ take y <$> M.getColumn x matrix
        , drop (y + 1) <$> M.getColumn x matrix
        , take x <$> M.getRow y matrix
        , drop (x + 1) <$> M.getRow y matrix
        ]
  let visibleTrees = count identity $ fromFoldable $ M.indexedMap isVisible matrix

  Right $ show visibleTrees

solveExtra :: String -> Either String String
solveExtra input = do
  grid <- note "failed to build grid" $ sequence $
    traverse Int.fromString <$> split (Pattern "") <$> inputToRows input
  matrix <- note "failed to create matrix" $ M.fromArray grid
  let sceneticScore height arr = product $ countUntil (_ >= height) <$> arr
  let score x y height = sceneticScore height $
        [ reverse $ take y $ fromMaybe [] $ M.getColumn x matrix
        , drop (y + 1) $ fromMaybe [] $ M.getColumn x matrix
        , reverse $ take x $ fromMaybe [] $ M.getRow y matrix
        , drop (x + 1) $ fromMaybe [] $ M.getRow y matrix
        ]
  Right $ show $ maximum $ M.indexedMap score matrix

countUntil :: forall a. (a -> Boolean) -> Array a -> Int
countUntil cond = countUntilRec
  where
    countUntilRec arr = case uncons arr of
      Just { head, tail } -> if cond head then 1 else 1 + countUntilRec tail
      Nothing -> 0
