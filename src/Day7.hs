module Day7 where

import Data.Function ((&))
import Data.List.Split
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char
import Lib
import Text.ParserCombinators.ReadP as P
import Data.Maybe (catMaybes)

{-
--- Day 7: Handy Haversacks ---

You land at the regional airport in time for your next flight. In fact, it looks like you'll even have time to grab some food: all flights are currently delayed due to issues in luggage processing.

Due to recent aviation regulations, many rules (your puzzle input) are being enforced about bags and their contents; bags must be color-coded and must contain specific quantities of other color-coded bags. Apparently, nobody responsible for these regulations considered how long they would take to enforce!

For example, consider the following rules:

light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.

These rules specify the required contents for 9 bag types. In this example, every faded blue bag is empty, every vibrant plum bag contains 11 bags (5 faded blue and 6 dotted black), and so on.

You have a shiny gold bag. If you wanted to carry it in at least one other bag, how many different bag colors would be valid for the outermost bag? (In other words: how many colors can, eventually, contain at least one shiny gold bag?)

In the above rules, the following options would be available to you:

    A bright white bag, which can hold your shiny gold bag directly.
    A muted yellow bag, which can hold your shiny gold bag directly, plus some other bags.
    A dark orange bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag.
    A light red bag, which can hold bright white and muted yellow bags, either of which could then hold your shiny gold bag.

So, in this example, the number of bag colors that can eventually contain at least one shiny gold bag is 4.

How many bag colors can eventually contain at least one shiny gold bag? (The list of rules is quite long; make sure you get all of it.)

To begin, get your puzzle input.


-}

newtype Bag = Bag String
  deriving (Eq, Show, Ord)

type Container = Map.Map Bag (Set.Set (Integer, Bag))

parse :: [String] -> Container
parse ls = ls
         & map (quickParse bagParser)
         & catMaybes
         & Map.fromList

bagParser :: ReadP (Bag, Set.Set(Integer, Bag))
bagParser = do
  -- light red bags contain 1 bright white bag, 2 muted yellow bags.
  container <- bag True
  string " contain "
  contents <- contentParser
  return (container, contents)

bag :: Bool -> ReadP Bag
bag plural = do
  kind <- ((munch1 isAlpha) <<>> char ' ' <<>> (munch1 isAlpha)) <<!! (\ ((s1, _), s2) -> s1 ++ " " ++ s2)
  char ' '
  if plural then do
    string "bags"
  else do
    string "bag"
  return $ Bag kind

contentParser :: ReadP (Set.Set (Integer, Bag))
contentParser = do
  (do
    bags <- P.sepBy contentParser' (string ", ")
    char '.'
    eof
    return $ Set.fromList bags)
  <++
  (do
    string "no other bags."
    eof
    return Set.empty)

contentParser' :: ReadP (Integer, Bag)
contentParser' = do
  n <- natParser
  char ' '
  b <- bag (n /= 1)
  return (n, b)


myBag = Bag "shiny gold"



-- What things can contain a particular bag, directly
type Containment = Map.Map Bag (Set.Set Bag)

invert :: Container -> Containment
invert m =
  let bagToBags = Map.map dropNumbers m :: Map.Map Bag (Set.Set Bag)
  in  mapReverseAll bagToBags
  where
    dropNumbers :: Set.Set (Integer, Bag) -> Set.Set Bag
    dropNumbers s = Set.map snd s

canContain :: Container -> Bag -> Set.Set Bag
canContain c b =
  case Map.lookup b inv of
       Nothing -> Set.empty
       Just es -> canContain' es
  where
    inv :: Containment
    inv = invert c
    canContain' :: Set.Set Bag -> Set.Set Bag
    canContain' es =
      let es' = (Set.map expand es & Set.unions) `Set.union` es
      in  if es == es'
          then es
          else canContain' es'
    expand :: Bag -> Set.Set Bag
    expand e =
      case Map.lookup e inv of
        Nothing -> Set.empty
        Just es' -> es'

day7 ls =
  let input = parse ls
  in canContain input myBag & Set.size


{-
--- Part Two ---

It's getting pretty expensive to fly these days - not because of ticket prices, but because of the ridiculous number of bags you need to buy!

Consider again your shiny gold bag and the rules from the above example:

    faded blue bags contain 0 other bags.
    dotted black bags contain 0 other bags.
    vibrant plum bags contain 11 other bags: 5 faded blue bags and 6 dotted black bags.
    dark olive bags contain 7 other bags: 3 faded blue bags and 4 dotted black bags.

So, a single shiny gold bag must contain 1 dark olive bag (and the 7 bags within it) plus 2 vibrant plum bags (and the 11 bags within each of those): 1 + 1*7 + 2 + 2*11 = 32 bags!

Of course, the actual rules have a small chance of going several levels deeper than this example; be sure to count all of the bags, even if the nesting becomes topologically impractical!

Here's another example:

shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.

In this example, a single shiny gold bag must contain 126 other bags.

How many individual bags are required inside your single shiny gold bag?
-}

totalForward :: Container -> Bag -> Integer
totalForward c b =
  case Map.lookup b c of
    Nothing -> 1        -- just the bag itself
    Just nbs -> 1 + sum [n * totalForward c b' | (n, b') <- Set.toList nbs]   -- this bag plus what it holds

day7b ls =
  let input = parse ls
  in  totalForward input myBag - 1

