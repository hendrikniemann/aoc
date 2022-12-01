module Advent.Day4
  ( solve
  , solveExtra
  )
  where

import Prelude

import Data.Array (dropWhile, filter, find, fromFoldable, mapMaybe, tail, uncons)
import Data.Either (Either(..), note)
import Data.Foldable (all, any, foldM)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split, trim)
import Data.String.Regex as Regex
import Data.Traversable (sum, traverse)
import Data.Tuple (Tuple(..))
import Matrix (Matrix, columns, fromArray, rows)
import Util (inputToRows, unsafeSimpleRegex)


solve :: String -> Either String String
solve input = do
  game <- note "Could not parse input successfully" $ parseGame input

  case playRounds game.boards game.numbers of
    Left (Tuple winner number) -> Right $ show $ score number winner
    Right _ -> Left "Could not determine a winner."


solveExtra :: String -> Either String String
solveExtra input = do
  game <- note "Could not parse input successfully" $ parseGame input

  case playLoosingRounds game.boards game.numbers of
    Left (Tuple winner number) -> Right $ show $ score number winner
    Right _ -> Left "Could not determine a winner."


parseGame :: String -> Maybe { boards :: Array Board, numbers :: Array Int }
parseGame input = do
  { head: inNumbers, tail: inBoards } <- uncons $ filter (_ /= "") $ split (Pattern "\n\n") input

  numbers <- traverse fromString $ split (Pattern ",") inNumbers
  matrices <- traverse readMatrix inBoards
  let boards = map { drawn: false, value: _ } <$> matrices

  pure { numbers, boards }


type Board = Matrix { drawn :: Boolean, value :: Int }


-- | Plays the game and returns (Left (Tuple winner lastNumber)) if a winner is found within the
-- | moves played or (Right boards) when no winner has been found after the provided moves
playRounds :: Array Board -> Array Int -> Either (Tuple Board Int) (Array Board)
playRounds initBoards rounds = foldM playSingleRound initBoards rounds
  where
    playSingleRound :: Array Board -> Int -> Either (Tuple Board Int) (Array Board)
    playSingleRound boards number =
      let
        boards' = map (draw number) boards
      in
        case find hasWon boards' of
          Just winner -> Left $ Tuple winner number
          Nothing -> Right boards'


playLoosingRounds :: Array Board -> Array Int -> Either (Tuple Board Int) (Array Board)
playLoosingRounds initBoards rounds =
    case foldM playLoosingRound initBoards rounds of
      Left (Tuple looserBoard latestNum) -> do
        let remaining = tail $ dropWhile (_ /= latestNum) rounds
        case remaining of
          Nothing -> Right [looserBoard]
          Just remainingRounds -> playRounds [looserBoard] remainingRounds
      final -> final
  where
    playLoosingRound :: Array Board -> Int -> Either (Tuple Board Int) (Array Board)
    playLoosingRound boards number =
      let
        boards' = map (draw number) boards
      in
        case filter (not <<< hasWon) boards' of
          [winner] -> Left $ Tuple winner number
          xs -> Right xs


readMatrix :: String -> Maybe (Matrix Int)
readMatrix str = do
  let strings = Regex.split (unsafeSimpleRegex " +") <$> trim <$> inputToRows str
  fromArray strings >>= traverse fromString


draw :: Int -> Board -> Board
draw num board = board <#> \field -> if field.value == num then field { drawn = true } else field


hasWon :: Board -> Boolean
hasWon board = any (all _.drawn) (rows board) || any (all _.drawn) (columns board)


score :: Int -> Board -> Int
score number = fromFoldable >>> mapMaybe winnerNumber >>> sum >>> (_ * number)
  where
    winnerNumber { drawn: false, value } = Just value
    winnerNumber _ = Nothing
