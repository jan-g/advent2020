module Day16 where

import Data.Function ((&))
import Data.List.Split
import Data.List as L
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe (catMaybes, isJust, fromJust)
import Text.ParserCombinators.ReadP as P
import Numeric (readInt)
import Data.Bits ((.&.), (.|.))

import Lib

{-
--- Day 16: Ticket Translation ---

As you're walking to yet another connecting flight, you realize that one of the legs of your re-routed trip coming up is on a high-speed train. However, the train ticket you were given is in a language you don't understand. You should probably figure out what it says before you get to the train station after the next flight.

Unfortunately, you can't actually read the words on the ticket. You can, however, read the numbers, and so you figure out the fields these tickets must have and the valid ranges for values in those fields.

You collect the rules for ticket fields, the numbers on your ticket, and the numbers on other nearby tickets for the same train service (via the airport security cameras) together into a single document you can reference (your puzzle input).

The rules for ticket fields specify a list of fields that exist somewhere on the ticket and the valid ranges of values for each field. For example, a rule like class: 1-3 or 5-7 means that one of the fields in every ticket is named class and can be any value in the ranges 1-3 or 5-7 (inclusive, such that 3 and 5 are both valid in this field, but 4 is not).

Each ticket is represented by a single line of comma-separated values. The values are the numbers on the ticket in the order they appear; every ticket has the same format. For example, consider this ticket:

.--------------------------------------------------------.
| ????: 101    ?????: 102   ??????????: 103     ???: 104 |
|                                                        |
| ??: 301  ??: 302             ???????: 303      ??????? |
| ??: 401  ??: 402           ???? ????: 403    ????????? |
'--------------------------------------------------------'

Here, ? represents text in a language you don't understand. This ticket might be represented as 101,102,103,104,301,302,303,401,402,403; of course, the actual train tickets you're looking at are much more complicated. In any case, you've extracted just the numbers in such a way that the first number is always the same specific field, the second number is always a different specific field, and so on - you just don't know what each position actually means!

Start by determining which tickets are completely invalid; these are tickets that contain values which aren't valid for any field. Ignore your ticket for now.

For example, suppose you have the following notes:

class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12

It doesn't matter which position corresponds to which field; you can identify invalid nearby tickets by considering only whether tickets contain values that are not valid for any field. In this example, the values on the first nearby ticket are all valid for at least one field. This is not true of the other three nearby tickets: the values 4, 55, and 12 are are not valid for any field. Adding together all of the invalid values produces your ticket scanning error rate: 4 + 55 + 12 = 71.

Consider the validity of the nearby tickets you scanned. What is your ticket scanning error rate?

To begin, get your puzzle input.
-}

parse ls =
  let [classes, myTicket, nearby] = splitOn [""] ls
      cs = map (quickParse parseClass) classes & catMaybes
      classMap = Map.fromList [(fieldName c, c) | c <- cs]
      mine = myTicket !! 1 & parseTicket
      theirs = tail nearby & map parseTicket
  in (classMap, mine, theirs)

type Ticket = [Integer]
parseTicket :: String -> Ticket
parseTicket s = s & splitOn "," & map read

type Validator = Integer -> Bool
data Class = Class String Validator
instance Show Class where
  show = fieldName
instance Eq Class where
  x == y = (fieldName x) == (fieldName y)
instance Ord Class where
  compare x y = compare (fieldName x) (fieldName y)

fieldName (Class n _) = n
validator (Class _ v) = v


parseClass :: ReadP Class
parseClass = do
  field <- many1 (satisfy (/= ':'))
  string ": "
  validator <- parseValidator
  eof
  return $ Class field validator

parseValidator :: ReadP Validator
parseValidator = do
  vs <- P.sepBy parseRange (string " or ")
  return $ \n -> any (\v -> v n) vs

parseRange :: ReadP Validator
parseRange = do
  from <- natParser
  char '-'
  to <- natParser
  return $ \n -> from <= n && n <= to


day16 ls =
  let (fs, me, them) = parse ls
  in sum (allInvalids fs them)


anyMatches :: Map.Map String Class -> Integer -> Map.Map String Class
anyMatches fs n =
  Map.filter (`validator` n) fs

matchesAny :: Map.Map String Class -> Integer -> Bool
matchesAny fs n =
  Map.size (anyMatches fs n) /= 0

anyMatchAllFields :: Map.Map String Class -> Ticket -> Bool
anyMatchAllFields fs ns =
  all (matchesAny fs) ns

initialValidation :: Map.Map String Class -> [Ticket] -> ([Ticket], [Ticket])
initialValidation fs ts = partition (anyMatchAllFields fs) ts

allInvalids :: Map.Map String Class -> [Ticket] -> [Integer]
allInvalids fs ts = concat ts & filter (not . matchesAny fs)

{-
--- Part Two ---

Now that you've identified which tickets contain invalid values, discard those tickets entirely. Use the remaining valid tickets to determine which field is which.

Using the valid ranges for each field, determine what order the fields appear on the tickets. The order is consistent between all tickets: if seat is the third field, it is the third field on every ticket, including your ticket.

For example, suppose you have the following notes:

class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9

Based on the nearby tickets in the above example, the first position must be row, the second position must be class, and the third position must be seat; you can conclude that in your ticket, class is 12, row is 11, and seat is 13.

Once you work out which field is which, look for the six fields on your ticket that start with the word departure. What do you get if you multiply those six values together?
-}

day16b ls =
  let (fs, me, them) = parse ls
      (valids, _) = initialValidation fs them
  in partB fs me valids

dictToSet d = Map.toList d & map snd & Set.fromList

candidatesForFieldValue :: Set.Set Class -> Integer -> Set.Set Class
candidatesForFieldValue fs n = Set.filter (\c -> validator c n) fs

candidatesForFieldValues :: Set.Set Class -> [Integer] -> Set.Set Class
candidatesForFieldValues fs ns = foldl candidatesForFieldValue fs ns

-- given a list of tickets, work out the candidates for each field in turn and return that list
candidatesForTickets :: Set.Set Class -> [Ticket] -> [Set.Set Class]
candidatesForTickets fs ts =
  let fields = transpose ts
  in  map (candidatesForFieldValues fs) fields

-- given a list of candidate sets, winnow them down into unique ones
winnowCandidateSets :: [Set.Set Class] -> [Set.Set Class]
winnowCandidateSets css =
  let singletons = filter (\cs -> Set.size cs == 1) css
  in  if length singletons == 0 then error "no unique solution"
      else if length singletons == length css then css
      else
        let narrowed = [narrow singletons cs | cs <- css]
        in winnowCandidateSets narrowed
  where narrow singletons cs
          | Set.size cs == 1 = cs
          | otherwise        = foldl Set.difference cs singletons

partB :: Map.Map String Class -> Ticket -> [Ticket] -> Integer
partB cs me them =
  let fs = dictToSet cs
      cands = candidatesForTickets fs them
      final = winnowCandidateSets cands
      final' = map (\cs -> Set.toList cs & head) final  -- list of classes
      mine = zip final' me  -- my ticket entries together with their corresponding class
      departure = filter (\(c, value) -> isPrefixOf "departure" $ fieldName c) mine
  in  departure & map snd & product