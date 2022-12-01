module Advent.Day11
  ( solve
  , solveExtra
  )
  where


import Prelude

import Control.Monad.State (State, execState, modify_)
import Data.Array (filter, foldM, foldl, length, (..))
import Data.Either (Either(..), note)
import Data.Int (fromString)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Matrix as M
import Util (inputToRows)


solve :: String -> Either String String
solve input = do
  start <- parseInput input

  let count = flip execState 0 $ foldM simulateRoundState start (1 .. 100)

  pure $ show count
    where
      simulateRoundState :: M.Matrix Int -> Int -> State Int (M.Matrix Int)
      simulateRoundState m _ = do
        let Tuple count m' = simulateRound m
        modify_ (_ + count)
        pure m'


solveExtra :: String -> Either String String
solveExtra input = do
  start <- parseInput input

  count <- case foldM simulateRoundEither start (1 .. 500) of
    Left x -> pure x
    Right _ -> Left "Could not find all flashing within 500 rounds"

  pure $ show count
    where
      simulateRoundEither :: M.Matrix Int -> Int -> Either Int (M.Matrix Int)
      simulateRoundEither m round = do
        let Tuple count m' = simulateRound m
        if count == 100 then Left round else pure m'


parseInput :: String -> Either String (M.Matrix Int)
parseInput input = do
  rows <-
    note "Could not turn all chars to ints" $
    traverse (traverse fromString) $
    split (Pattern "") <$> inputToRows input
  note "line lengths differ" $ M.fromArray rows


simulateRound :: M.Matrix Int -> Tuple Int (M.Matrix Int)
simulateRound m = do
  let increased = map (_ + 1) m
  flash increased
    where
    flash :: M.Matrix Int -> Tuple Int (M.Matrix Int)
    flash field =
      let
        flashedFields = filter (\{ value } -> value > 9) $ M.toIndexedArray field
      in
        if length flashedFields == 0 then
          Tuple 0 field
        else
            let
              field' = foldl (propagateFlash) field flashedFields
              Tuple acc field'' = flash field'
            in
              Tuple (acc + length flashedFields) field''

    inc :: Int -> Int
    inc x = if x > 9 || x == 0 then x else x + 1

    propagateFlash :: forall r. M.Matrix Int -> { x :: Int, y :: Int | r } -> M.Matrix Int
    propagateFlash o { x, y } =
      foldl (modifyIgnore inc) (fromMaybe o (M.set x y 0 o)) (adjectendFields x y)

    modifyIgnore :: forall a. (a -> a) -> M.Matrix a -> { x :: Int, y :: Int } -> M.Matrix a
    modifyIgnore f matrix { x, y } = fromMaybe matrix $ M.modify x y f matrix

    adjectendFields :: Int -> Int -> Array { x :: Int, y :: Int }
    adjectendFields x y =
      [ { x: x - 1, y: y - 1 }
      , { x, y: y - 1 }
      , { x: x + 1, y: y - 1 }
      , { x: x - 1, y }
      , { x: x + 1, y }
      , { x: x - 1, y: y + 1 }
      , { x, y: y + 1 }
      , { x: x + 1, y: y + 1 }
      ]