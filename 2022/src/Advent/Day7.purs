module Advent.Day7
  ( solve
  , solveExtra
  )
  where


import Prelude

import Data.Either (Either(..), note)
import Data.Foldable (sum)
import Data.Int as Int
import Data.List (List(..), drop, fromFoldable, length, takeWhile, (:))
import Data.Map as Map
import Data.String (Pattern(..), split)
import Data.String.Utils (startsWith)
import Data.Traversable (for)
import Debug (spy, spyWith)
import Util (inputToRows)


data FileOrDir = Dir String | File String Int


size :: Map.Map String Int -> FileOrDir -> Either String Int
size _ (File _ s) = pure s
size m (Dir name) = note "Could not find dir in map" $ Map.lookup name m


buildStructure :: List String -> Either String (Map.Map String Int)
buildStructure Nil = pure Map.empty
buildStructure ("$ cd ..":list) = buildStructure list
buildStructure (cd:"$ ls":list) =
  let
    content = takeWhile (not <<< startsWith "$ cd") list
    rest = drop (length content) list
  in do
    name <- case split (Pattern " ") cd of
      ["$", "cd", folderName] -> pure folderName
      _ -> Left "Could not find out current folder name"
    fileMap <- buildStructure rest
    folderSize <- sum <$> content `for` (parseRow >=> size fileMap)
    _ <- if Map.member name fileMap then Left ("dublicate key" <> name) else pure unit
    pure $ Map.insert name folderSize fileMap
buildStructure _ = Left "Unexpected list shape"

parseRow :: String -> Either String FileOrDir
parseRow row = case split (Pattern " ") row of
  ["dir", name] -> pure $ Dir name
  [filesize, name] -> File name <$> (note "Could not parse file size" $ Int.fromString filesize)
  _ -> Left "Could not parse row"


solve :: String -> Either String String
solve input = do
  let rows = fromFoldable $ inputToRows """$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"""
  fileMap <- buildStructure rows
  pure $ show $ sum $ spyWith "filtered" show $ Map.filter (_ <= 100000) fileMap


solveExtra :: String -> Either String String
solveExtra input = do
  Left "Unsolved"
