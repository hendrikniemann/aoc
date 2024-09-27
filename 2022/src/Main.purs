module Main where

import Prelude

import Advent.Day1 as Day1
import Advent.Day2 as Day2
import Advent.Day3 as Day3
import Advent.Day4 as Day4
import Advent.Day5 as Day5
import Advent.Day6 as Day6
import Advent.Day7 as Day7
import Advent.Day8 as Day8
import Data.Either (Either(..), either, note)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String (stripSuffix, Pattern(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (error, runAff_, throwError)
import Effect.Console as Console
import FetchApi (fetchInput)
import Node.Process as Process
import Node.ReadLine (Interface, createConsoleInterface, noCompletion, question)


problems :: Map.Map String (String -> Either String String)
problems = Map.fromFoldable
  [ Tuple "1" Day1.solve
  , Tuple "1+" Day1.solveExtra
  , Tuple "2" Day2.solve
  , Tuple "2+" Day2.solveExtra
  , Tuple "3" Day3.solve
  , Tuple "3+" Day3.solveExtra
  , Tuple "4" Day4.solve
  , Tuple "4+" Day4.solveExtra
  , Tuple "5" Day5.solve
  , Tuple "5+" Day5.solveExtra
  , Tuple "6" Day6.solve
  , Tuple "6+" Day6.solveExtra
  , Tuple "7" Day7.solve
  , Tuple "7+" Day7.solveExtra
  , Tuple "8" Day8.solve
  , Tuple "8+" Day8.solveExtra
  -- , Tuple "9" Day9.solve
  -- , Tuple "9+" Day9.solveExtra
  -- , Tuple "10" Day10.solve
  -- , Tuple "10+" Day10.solveExtra
  -- , Tuple "11" Day11.solve
  -- , Tuple "11+" Day11.solveExtra
  -- , Tuple "12" Day12.solve
  -- , Tuple "12+" Day12.solveExtra
  -- , Tuple "13" Day13.solve
  -- , Tuple "13+" Day13.solveExtra
  -- , Tuple "14" Day14.solve
  -- , Tuple "14+" Day14.solveExtra
  ]


main :: Effect Unit
main = do
  interface <- createConsoleInterface noCompletion
  selectDay interface


selectDay :: Interface -> Effect Unit
selectDay interface = question text processAnswer interface
  where
    text = "Which day's exercise would you like to run?\n>"
    processAnswer result =
      if Map.member result problems then
        executeProblem result
      else do
        Console.log "Please provide a day between between 1 and 24 and optionally a + at the end!"
        selectDay interface


executeProblem :: String -> Effect Unit
executeProblem problem = runAff_ reportResult do
  let day = fromMaybe problem $ stripSuffix (Pattern "+") problem
  input <- fetchInput day
  either (error >>> throwError) pure $ do
    solve <- note ("Problem " <> day <> " has no solution yet") $ Map.lookup problem problems
    solve input
  where
    reportResult (Left error) = do
      Console.error $ show error
      Process.exit 1
    reportResult (Right result) = do
      Console.log $ "The result is: " <> result
      Process.exit 0
