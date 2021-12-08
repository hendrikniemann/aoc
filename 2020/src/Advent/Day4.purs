module Advent.Day4 where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (toNonEmpty)
import Data.Either (Either)
import Data.Foldable (and)
import Data.Int as Int
import Data.List (any)
import Data.Map (Map, fromFoldable, lookup, member)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty ((:|))
import Data.String (Pattern(..), split)
import Data.String.Regex (match, split, test) as Regex
import Data.Tuple (Tuple(..))
import Util (count, unsafeSimpleRegex)


type Passport = Map String String


solve :: String -> Either String String
solve =
  split (Pattern "\n\n") >>>
  Array.filter (_ /= "") >>>
  map parsePassport >>>
  count isPassportValid2 >>>
  show >>>
  pure


isPassportValid1 :: Passport -> Boolean
isPassportValid1 passport = and $ map (_ `member` passport) requiredKeys
  where
    requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]


isPassportValid2 :: Passport -> Boolean
isPassportValid2 passport = fromMaybe false ado
  byr <- Int.fromString =<< lookup "byr" passport
  iyr <- Int.fromString =<< lookup "iyr" passport
  eyr <- Int.fromString =<< lookup "eyr" passport
  hgt <- lookup "hgt" passport
  hcl <- lookup "hcl" passport
  ecl <- lookup "ecl" passport
  pid <- lookup "pid" passport
  in and
      [ between 1920 2002 byr
      , between 2010 2020 iyr
      , between 2020 2030 eyr
      , isHeight hgt
      , isHairColor hcl
      , any (_ == ecl) ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
      , isPassportNumber pid
      ]


parsePassport :: String -> Passport
parsePassport passport = fromFoldable $ Array.mapMaybe parseKV $ Regex.split reg passport
  where
    parseKV str = case split (Pattern ":") str of
      [key, value] -> Just (Tuple key value)
      _ -> Nothing
    reg = unsafeSimpleRegex "\n| "


isHairColor :: String -> Boolean
isHairColor = Regex.test (unsafeSimpleRegex "#[0-9a-f]{6}$")


isPassportNumber :: String -> Boolean
isPassportNumber = Regex.test (unsafeSimpleRegex "^[0-9]{9}$")


isHeight :: String -> Boolean
isHeight str = case toNonEmpty <$> Regex.match (unsafeSimpleRegex "^([1-9][0-9]*)(cm|in)$") str of
  Just (_ :| [Just val, Just "in"]) -> between 59 76 $ fromMaybe 0 $ Int.fromString val
  Just (_ :| [Just val, Just "cm"]) -> between 150 193 $ fromMaybe 0 $ Int.fromString val
  _ -> false
