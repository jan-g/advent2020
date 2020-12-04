module Day4 where

import Data.Function ((&))
import Data.List.Split (splitOn)
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe (catMaybes, isJust)
import Text.ParserCombinators.ReadP

{-
--- Day 4: Passport Processing ---

You arrive at the airport only to realize that you grabbed your North Pole Credentials instead of your passport. While these documents are extremely similar, North Pole Credentials aren't issued by a country and therefore aren't actually valid documentation for travel in most of the world.

It seems like you're not the only one having problems, though; a very long line has formed for the automatic passport scanners, and the delay could upset your travel itinerary.

Due to some questionable network security, you realize you might be able to solve both of these problems at the same time.

The automatic passport scanners are slow because they're having trouble detecting which passports have all required fields. The expected fields are as follows:

    byr (Birth Year)
    iyr (Issue Year)
    eyr (Expiration Year)
    hgt (Height)
    hcl (Hair Color)
    ecl (Eye Color)
    pid (Passport ID)
    cid (Country ID)

Passport data is validated in batch files (your puzzle input). Each passport is represented as a sequence of key:value pairs separated by spaces or newlines. Passports are separated by blank lines.

Here is an example batch file containing four passports:

ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in

The first passport is valid - all eight fields are present. The second passport is invalid - it is missing hgt (the Height field).

The third passport is interesting; the only missing field is cid, so it looks like data from North Pole Credentials, not a passport at all! Surely, nobody would mind if you made the system temporarily ignore missing cid fields. Treat this "passport" as valid.

The fourth passport is missing two fields, cid and byr. Missing cid is fine, but missing any other field is not, so this passport is invalid.

According to the above rules, your improved system would report 2 valid passports.

Count the number of valid passports - those that have all required fields. Treat cid as optional. In your batch file, how many passports are valid?

To begin, get your puzzle input.
-}

parse :: [String] -> [Passport]
parse ls =
  let groups = splitOn [""] ls
      combined = map (concatMap (++ " ")) groups
  in  map parsePassport combined & catMaybes

data Passport = Passport { byr :: Maybe String
                         , iyr :: Maybe String
                         , eyr :: Maybe String
                         , hgt :: Maybe String
                         , hcl :: Maybe String
                         , ecl :: Maybe String
                         , pid :: Maybe String
                         , cid :: Maybe String
                         }
  deriving (Show, Eq)

nullPassport = Passport { byr = Nothing
                        , iyr = Nothing
                        , eyr = Nothing
                        , hgt = Nothing
                        , hcl = Nothing
                        , ecl = Nothing
                        , pid = Nothing
                        , cid = Nothing
                        }     

parsePassport :: String -> Maybe Passport
parsePassport s =
  case readP_to_S lineParser s of
    [] -> Nothing
    [(m, "")] -> Just m

lineParser :: ReadP Passport
lineParser = do
  skipSpaces
  toks <- sepBy1 parseToken (skipMany1 $ char ' ')
  skipSpaces
  eof
  return $ foldl (\p f -> f p) nullPassport toks

parseToken :: ReadP (Passport -> Passport)
parseToken = do
  field <- munch1 isAlpha
  char ':'
  value <- munch1 (not . isSpace)
  return $ case field of
    "byr" -> \p -> p { byr=Just value }
    "iyr" -> \p -> p { iyr=Just value }
    "eyr" -> \p -> p { eyr=Just value }
    "hgt" -> \p -> p { hgt=Just value }
    "hcl" -> \p -> p { hcl=Just value }
    "ecl" -> \p -> p { ecl=Just value }
    "pid" -> \p -> p { pid=Just value }
    "cid" -> \p -> p { cid=Just value }
    -- _ -> error $ "unknown field:" ++ field ++ ":" ++ value

isValid p =
  isJust (byr p) &&
  isJust (iyr p) &&
  isJust (eyr p) &&
  isJust (hgt p) &&
  isJust (hcl p) &&
  isJust (ecl p) &&
  isJust (pid p)

day4 ls =
  let passports = parse ls
  in filter isValid passports & length

{-
-}

day4b ls = "hello world"
