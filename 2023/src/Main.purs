module Main where

import Prelude

import Advent.Day1 as Day1
import Data.Either (Either(..), either, note)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String (stripSuffix, Pattern(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, error, runAff_, throwError)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class.Console as Console
import FetchApi (fetchInput)
import Node.Process as Process


foreign import questionFfi :: String -> EffectFnAff String

question :: String -> Aff String
question = fromEffectFnAff <<< questionFfi

type Problem = String -> Either String String

problems :: Map.Map String Problem
problems = Map.fromFoldable
  [ Tuple "1" Day1.solve
  , Tuple "1+" Day1.solveExtra
  ]


main :: Effect Unit
main = do
  runAff_ reportResult do
    Console.log "Fuck"
    problem <- selectDay
    executeProblem problem
  where
    reportResult (Left error) = do
      Console.error $ show error
      Process.exit' 1
    reportResult (Right result) = do
      Console.log $ "The result is: " <> result
      Process.exit' 0


selectDay :: Aff String
selectDay = do
  result <- question "Which day's exercise would you like to run?\n>"
  if Map.member result problems then
    pure result
  else do
    _ <- (Console.log "Please provide a day between between 1 and 24 and optionally a + at the end!" :: Aff Unit)
    selectDay


executeProblem :: String -> Aff String
executeProblem problem = do
  let day = fromMaybe problem $ stripSuffix (Pattern "+") problem
  input <- fetchInput day
  either (error >>> throwError) pure $ do
    solve <- note ("Problem " <> day <> " has no solution yet") $ Map.lookup problem problems
    solve input
