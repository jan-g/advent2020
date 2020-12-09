module Day10 where

import Data.Function ((&))
import Data.List.Split
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe (catMaybes, isJust, fromJust)
import Text.ParserCombinators.ReadP as P

import Lib

{-
-}

parse ls = ls
         & head
         & splitOn "-"
         & map read

day10 ls = "hello world"

{-
-}

day10b ls = "hello world"
