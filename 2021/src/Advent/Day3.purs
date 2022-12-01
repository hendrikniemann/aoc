module Advent.Day3
  ( solve
  , solveExtra
  )
  where

import Prelude

import Data.Array (filter, foldM, intercalate, length, (!!), (..))
import Data.Either (Either(..), either, note)
import Data.Int (fromStringAs, radix)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sum)
import Matrix as Matrix


solve :: String -> Either String String
solve input = note "Could not turn row into int" do
  let rows = filter (_ /= "") $ split (Pattern "\n") input
  matrix <- readMatrix rows
  let average = averageBit <$> Matrix.columns matrix

  gamma <- rowToInt average
  epsilon <- rowToInt $ map flipBit average

  pure $ show $ gamma * epsilon

solveExtra :: String -> Either String String
solveExtra input = note "Could not turn row into int" do
  let rows = filter (_ /= "") $ split (Pattern "\n") input
  matrix <- readMatrix rows

  let findO2 m pos = do
        if Matrix.height m == 1 then Left m else Right unit
        let avg = averageBit <$> Matrix.getColumn pos m
        note m $ Matrix.fromArray $ filter ((_ !! pos) >>> eq avg) $ Matrix.rows m

  let findCO2 m pos = do
        if Matrix.height m == 1 then Left m else Right unit
        let avg = flipBit <$> averageBit <$> Matrix.getColumn pos m
        note m $ Matrix.fromArray $ filter ((_ !! pos) >>> eq avg) $ Matrix.rows m

  let extract = either Just (const Nothing) >=> Matrix.getRow 0 >=> rowToInt

  oxygen <- extract $ foldM findO2 matrix (0..12)
  co2 <- extract $ foldM findCO2 matrix (0..12)

  pure $ show $ oxygen * co2


readMatrix :: Array String -> Maybe (Matrix.Matrix Int)
readMatrix rows = map hackyToBit <$> (Matrix.fromArray $ toCharArray <$> rows)
  where
    hackyToBit '1' = 1
    hackyToBit _ = 0


averageBit :: Array Int -> Int
averageBit arr = if sum arr < ((length arr + (length arr `mod` 2)) / 2) then 0 else 1


rowToInt :: Array Int -> Maybe Int
rowToInt arr = do
  radix <- radix 2
  let asString = intercalate "" $ map show arr
  fromStringAs radix asString


flipBit :: Int -> Int
flipBit 1 = 0
flipBit _ = 1

