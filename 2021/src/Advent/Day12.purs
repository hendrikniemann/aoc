module Advent.Day12
  ( solve
  , solveExtra
  )
  where


import Prelude

import Control.Bind (bindFlipped)
import Data.Array (filter, length)
import Data.Either (Either(..))
import Data.List (List(..), elem, (:))
import Data.List as L
import Data.Set as Set
import Data.String (Pattern(..), split, toUpper)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd, swap)
import Util (inputToRows)


solve :: String -> Either String String
solve input = do
  connections <- traverse parseConnection $ inputToRows input
  let allConnections = connections <> (swap <$> connections)
  let paths = findPaths allConnections (Start:Nil)
  pure $ show $ length paths


solveExtra :: String -> Either String String
solveExtra input = do
  connections <- traverse parseConnection $ inputToRows input
  let allConnections = connections <> (swap <$> connections)
  let paths = findPathsExtras allConnections (Start:Nil)
  pure $ show $ length paths


data Node
  = Start
  | End
  | Large String
  | Small String


derive instance Eq Node


derive instance Ord Node


instance Show Node where
  show Start = "Start"
  show End = "End"
  show (Small s) = s
  show (Large s) = s

type Path = List Node


type Connection = Tuple Node Node


isSmallCave :: Node -> Boolean
isSmallCave (Small _) = true
isSmallCave _ = false

parseConnection :: String -> Either String Connection
parseConnection = split (Pattern "-") >>> case _ of
  [start, end] -> pure $ Tuple (parseNode start) (parseNode end)
  _ -> Left "Could not parse string to connection"


parseNode :: String -> Node
parseNode "start" = Start
parseNode "end" = End
parseNode s = if toUpper s == s then Large s else Small s


findPaths :: Array Connection -> Path -> Array Path
findPaths _ Nil = [] -- This is a useless case, we never call the function with an empty path
findPaths _ path@(End:_) = [path]
findPaths connections path@(currentCave:_) =
  let
    connectedCaves = map snd $ filter (fst >>> (_ == currentCave)) connections
    isAllowedCave = case _ of
      Start -> false
      node@(Small _) -> not $ elem node path
      _ -> true
    allowedCaves = filter isAllowedCave connectedCaves
  in
    -- If there are no connections and this path does not lead to the end throw it away
    if length allowedCaves == 0 then
      []
    else
      bindFlipped (findPaths connections) $ map (_:path) $ allowedCaves

findPathsExtras :: Array Connection -> Path -> Array Path
findPathsExtras _ Nil = [] -- This is a useless case, we never call the function with an empty path
findPathsExtras _ path@(End:_) = [path]
findPathsExtras connections path@(currentCave:_) =
  let
    connectedCaves = map snd $ filter (fst >>> (_ == currentCave)) connections
    smallCaves = L.filter isSmallCave path
    hasVisitedTwice = (L.length smallCaves) /= (Set.size $ Set.fromFoldable smallCaves)
    isAllowedCave = case _ of
      Start -> false
      node@(Small _) -> not hasVisitedTwice || not (elem node path)
      _ -> true
    allowedCaves = filter isAllowedCave connectedCaves
  in
    -- If there are no connections and this path does not lead to the end throw it away
    if length allowedCaves == 0 then
      []
    else
      bindFlipped (findPathsExtras connections) $ map (_:path) $ allowedCaves
