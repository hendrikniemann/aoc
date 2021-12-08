module Advent.Day5 where

import Prelude

import Control.Bind (bindFlipped)
import Data.Array as Array
import Data.Either (Either, note)
import Data.Foldable (maximum)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))


solve :: String -> Either String String
solve =
  split (Pattern "\n") >>>
  Array.filter (_ /= "") >>>
  (traverse parseBoardingPass >>> note "Unable to parse boarding passes") >>>
  map (map getPassId) >>>
  bindFlipped getMyBoardingPass >>>
  map show


data Indicator = Higher | Lower


fromChar :: Char -> Maybe Indicator
fromChar 'F' = Just Lower
fromChar 'L' = Just Lower
fromChar 'B' = Just Higher
fromChar 'R' = Just Higher
fromChar _ = Nothing


newtype BoardingPass = BoardingPass { row :: Int,  column :: Int }


getMaxBoardingPass :: Array Int -> Either String Int
getMaxBoardingPass = maximum >>> note "Empty array supplied"


getMyBoardingPass :: Array Int -> Either String Int
getMyBoardingPass passes =
  Array.range 0 (127*8+7) #
  Array.find missingPass #
  note "Unable to find missing pass"
    where
      set = Set.fromFoldable passes
      missingPass pass =
        Set.member (pass - 1) set &&
        Set.member (pass + 1) set &&
        not (Set.member pass set)


parseBoardingPass :: String -> Maybe BoardingPass
parseBoardingPass str = do
    parsedList <- traverse fromChar $ toCharArray str
    let rowList = Array.take 7 parsedList
    let columnList = Array.drop 7 parsedList
    let Tuple row _ = Array.foldl reduce (Tuple 0 127) rowList
    let Tuple column _ = Array.foldl reduce (Tuple 0 7) columnList
    pure $ BoardingPass { row, column }


getPassId :: BoardingPass -> Int
getPassId (BoardingPass { row, column }) = row * 8 + column


reduce :: Tuple Int Int -> Indicator -> Tuple Int Int
reduce (Tuple min max) Higher = Tuple (middle min max + 1) max
reduce (Tuple min max) Lower = Tuple min (middle min max)


middle :: Int -> Int -> Int
middle min max = (max - min) / 2 + min

