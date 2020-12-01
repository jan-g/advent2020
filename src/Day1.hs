module Day1 where

import Data.Function ((&))
import Data.List.Split
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char

{-
-}

parse ls = ls
         & head
         & splitOn "-"
         & map read

day1 ls = "hello world"

{-
-}

day1b ls = "hello world"
