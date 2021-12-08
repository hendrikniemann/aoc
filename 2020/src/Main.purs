module Main where

import Prelude

import Advent.Day1 as Day1
import Advent.Day2 as Day2
import Advent.Day3 as Day3
import Advent.Day4 as Day4
import Advent.Day5 as Day5
import Advent.Day6 as Day6
import Advent.Day7 as Day7
import Data.Either (Either(..), either, note)
import Data.Int (fromString) as Int
import Data.Map (fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (error, runAff_, throwError)
import Effect.Console as Console
import FetchApi (fetchInput)
import Node.Process as Process
import Node.ReadLine (Interface, createConsoleInterface, noCompletion, question)


main :: Effect Unit
main = do
  interface <- createConsoleInterface noCompletion
  selectDay interface


selectDay :: Interface -> Effect Unit
selectDay interface = question text processAnswer interface
  where
    text = "Which day's exercise would you like to run?\n>"
    processAnswer result = case Int.fromString result of
      Just day | day > 0 && day < 25 -> do
        executeProblem day
      _ -> do
        Console.log "Please provide an integer between 1 and 24!"
        selectDay interface


executeProblem :: Int -> Effect Unit
executeProblem day = runAff_ reportResult do
  input <- fetchInput day
  either (error >>> throwError) pure $ do
    solve <- note ("Problem for day " <> show day <> " has no solution yet") problem
    solve input
  where
    problem = lookup day $ fromFoldable
      [ Tuple 1 Day1.solve
      , Tuple 2 Day2.solve
      , Tuple 3 Day3.solve
      , Tuple 4 Day4.solve
      , Tuple 5 Day5.solve
      , Tuple 6 Day6.solve
      , Tuple 7 Day7.solve
      ]
    reportResult (Left error) = do
      Console.error $ show error
      Process.exit 1
    reportResult (Right result) = do
      Console.log $ "The result is: " <> result
      Process.exit 0
