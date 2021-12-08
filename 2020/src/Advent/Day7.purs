module Advent.Day7
  ( solve
  )
  where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Foldable (fold)
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (fromMaybe)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), split, trim)
import Data.String.CodeUnits as CodeUnits
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Debug (spy)
import Text.Parsing.StringParser (Parser, runParser) as Parser
import Text.Parsing.StringParser.CodeUnits (anyDigit, char, regex, string, whiteSpace) as Parser
import Text.Parsing.StringParser.Combinators (optional, sepBy1) as Parser


solve :: String -> Either String String
solve =
  split (Pattern "\n") >>>
  Array.filter (_ /= "") >>>
  map trim >>>
  traverse parseRule >>>
  map getRelation >>>
  spy "Result" >>>
  map (\_ -> "x")


newtype Rule = Rule { colour :: String, content :: Array Content }


derive instance genericRule :: Generic Rule _


instance showRule :: Show Rule where
  show = genericShow


newtype Content = Content { colour :: String, amount :: Int }


derive instance genericContent :: Generic Content _


instance showContent :: Show Content where
  show = genericShow


parseRule :: String -> Either String Rule
parseRule str = lmap (const $ "Could not parse: " <> str) $ Parser.runParser parserRule str
  where
    parserRule :: Parser.Parser Rule
    parserRule = ado
      colour <- parserColour
      Parser.string " bags contain "
      content <- manyBags <|> Parser.string "no other bags" *> pure []
      _ <- Parser.char '.'
      in Rule { colour, content }
        where
          manyBags = Array.fromFoldable <$> Parser.sepBy1 parserContent (Parser.string ", ")

    parserContent :: Parser.Parser Content
    parserContent = ado
      amount <- fromMaybe 0 <$> Int.fromString <$> CodeUnits.singleton <$> Parser.anyDigit
      _ <- Parser.whiteSpace
      colour <- parserColour
      _ <- Parser.whiteSpace *> Parser.string "bag" <* Parser.optional (Parser.char 's')
      in Content { amount, colour }

    parserColour :: Parser.Parser String
    parserColour = ado
      x <- Parser.regex "\\w+"
      _ <- Parser.whiteSpace
      y <- Parser.regex "\\w+"
      in x <> " " <> y


type Relation a = Set.Set (Tuple a a)


getRelation :: Array Rule -> Relation String
getRelation rules = fold $ map ruleToRelation rules


ruleToRelation :: Rule -> Relation String
ruleToRelation (Rule { colour, content }) = Set.fromFoldable $ map toPair content
  where
    toPair (Content { colour: contentColour }) = Tuple colour contentColour
