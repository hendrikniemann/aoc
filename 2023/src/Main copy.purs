module MainX where

import Prelude

import Advent.Day1 as Day1
import Data.Either (Either(..), either, note)
import Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.String (stripSuffix, Pattern(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, error, runAff_, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import FetchApi (fetchInput)
import Node.EventEmitter (on)
import Node.Process as Process
import Node.ReadLine (Interface, createConsoleInterface, lineH, noCompletion)
import Node.ReadLine.Aff (question)


type Problem = String -> Either String String

problems :: Map.Map String Problem
problems = Map.fromFoldable
  [ Tuple "1" Day1.solve
  , Tuple "1+" Day1.solveExtra
  ]


main :: Effect Unit
main = do
  interface <- createConsoleInterface noCompletion
  runAff_ reportResult do
    problem <- selectDay interface
    executeProblem problem
  where
    reportResult (Left error) = do
      Console.error $ show error
      Process.exit' 1
    reportResult (Right result) = do
      Console.log $ "The result is: " <> result
      Process.exit' 0


selectDay :: Interface -> Aff String
selectDay interface = do
  _ <- liftEffect $ interface # on lineH \line -> do
    Console.log $ "You selected: " <> line
  result <- question "Which day's exercise would you like to run?\n>" interface
  if Map.member result problems then
    pure result
  else do
    _ <- (Console.log "Please provide a day between between 1 and 24 and optionally a + at the end!" :: Aff Unit)
    selectDay interface


executeProblem :: String -> Aff String
executeProblem problem = do
  let day = fromMaybe problem $ stripSuffix (Pattern "+") problem
  input <- fetchInput day
  either (error >>> throwError) pure $ do
    solve <- note ("Problem " <> day <> " has no solution yet") $ Map.lookup problem problems
    solve input
