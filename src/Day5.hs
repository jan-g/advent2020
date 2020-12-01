module Day5 where

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

day5 ls = "hello world"

{-
-}

day5b ls = "hello world"
