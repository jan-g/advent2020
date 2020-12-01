module Lib
    ( loadLines
    , makeArray
    , natParser
    , intParser
    , drawMapWith
    , mapReverse
    , (<<<<)
    , (>>>>)
    , (<<>>)
    , (<<!!)
    , loadMap
    , loadMapWith
    , wordParser
    ) where

import Data.Array
import Data.Char
import Text.ParserCombinators.ReadP
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Function ((&))

loadLines fn = do
  contents <- readFile fn
  return (lines contents)

makeArray :: [a] -> Array Int a
makeArray ns = listArray (0, length ns - 1) ns

natParser :: ReadP Integer
natParser = do
  digits <- munch1 isDigit
  return $ read digits

intParser :: ReadP Integer
intParser = do
  (do
    char '-'
    i <- natParser
    return $ -i) <++ natParser

drawMapWith :: (Ord a, Enum a) => ((a, a) -> Maybe b -> c) -> Map.Map (a, a) b -> [[c]]
drawMapWith f m =
  let coords = Map.keysSet m
      xs = Set.map fst coords
      ys = Set.map snd coords
      (x0, x1) = (Set.findMin xs, Set.findMax xs)
      (y0, y1) = (Set.findMin ys, Set.findMax ys)
  in  [[f (x, y) (Map.lookup (x, y) m) | x <- [x0..x1]] | y <- [y0..y1]]

mapReverse :: (Ord v, Ord k) => Map.Map k v -> Map.Map v (Set.Set k)
mapReverse m = foldl (\m (k,v) -> Map.insertWith Set.union v (Set.singleton k) m) Map.empty (Map.toList m)


infixl 8 <<<<
(<<<<) :: ReadP p1 -> ReadP p2 -> ReadP p1
p1 <<<< p2 = do
  x <- p1
  _ <- p2
  return x

infixl 8 >>>>
(>>>>) :: ReadP p1 -> ReadP p2 -> ReadP p2
p1 >>>> p2 = do
  _ <- p1
  p2

infixl 6 <<>>
(<<>>) :: ReadP p1 -> ReadP p2 -> ReadP (p1, p2)
p1 <<>> p2 = do
  x <- p1
  y <- p2
  return (x, y)

infixl 8 <<!!
(<<!!) :: ReadP p1 -> (p1 -> p2) -> ReadP p2
p1 <<!! f = do
  x <- p1
  return $ f x

loadMap :: (Num a, Ord a, Enum a) => [String] -> Map.Map (a, a) Char
loadMap ls = [((x, y), c) | (y, line) <- [0..] `zip` ls, (x, c) <- [0..] `zip` line] & Map.fromList

loadMapWith :: (Num a, Ord a, Enum a) => ((a, a) -> Char -> b) -> [String] -> Map.Map (a, a) b
loadMapWith f ls = loadMap ls & Map.mapWithKey f

wordParser :: ReadP String
wordParser = many1 (satisfy (/= ' '))