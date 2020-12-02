module Day2 where

import Data.Function ((&))
import Data.List.Split
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe (catMaybes)

import Lib

import Text.ParserCombinators.ReadP


{-
--- Day 2: Password Philosophy ---

Your flight departs in a few days from the coastal airport; the easiest way down to the coast from here is via toboggan.

The shopkeeper at the North Pole Toboggan Rental Shop is having a bad day. "Something's wrong with our computers; we can't log in!" You ask if you can take a look.

Their password database seems to be a little corrupted: some of the passwords wouldn't have been allowed by the Official Toboggan Corporate Policy that was in effect when they were chosen.

To try to debug the problem, they have created a list (your puzzle input) of passwords (according to the corrupted database) and the corporate policy when that password was set.

For example, suppose you have the following list:

1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc

Each line gives the password policy and then the password. The password policy indicates the lowest and highest number of times a given letter must appear for the password to be valid. For example, 1-3 a means that the password must contain a at least 1 time and at most 3 times.

In the above example, 2 passwords are valid. The middle password, cdefg, is not; it contains no instances of b, but needs at least 1. The first and third passwords are valid: they contain one a or nine c, both within the limits of their respective policies.

How many passwords are valid according to their policies?

To begin, get your puzzle input.
-}

parse :: [String] -> [(Integer, Integer, Char, String)]
parse ls = ls & map parseLine & catMaybes

parseLine :: String -> Maybe (Integer, Integer, Char, String)
parseLine s =
  case readP_to_S lineParser s of
    [] -> Nothing
    [(m, "")] -> Just m

lineParser :: ReadP (Integer, Integer, Char, String)
lineParser = do
  lower <- natParser
  char '-'
  upper <- natParser
  char ' '
  c <- get
  string ": "
  pass <- munch1 isAlpha
  eof
  return (lower, upper, c, pass)

day2 ls =
  let lines = parse ls
  in  filter (\ (lower, upper, c, pass) ->
                let n = filter (== c) pass & length & fromIntegral
                in  lower <= n && n <= upper
             ) lines &
      length

{-
-}

day2b ls = "hello world"
