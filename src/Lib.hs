module Lib
    ( loadLines
    , makeArray
    , natParser
    , intParser
    , quickParse
    , drawMapWith
    , mapReverse
    , mapReverseAll
    , (<<<<)
    , (>>>>)
    , (<<>>)
    , (<<!!)
    , loadMap
    , loadMapWith
    , wordParser
    , euc
    , solveDiophantine
    , combineDiophantine
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

quickParse :: ReadP t -> String -> Maybe t
quickParse parser s =
  case readP_to_S parser s of
    [] -> Nothing
    [(m, "")] -> Just m
    _ -> Nothing

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

mapReverseAll :: (Ord v, Ord k) => Map.Map k (Set.Set v) -> Map.Map v (Set.Set k)
mapReverseAll m =
  let kvs = Map.map Set.toList m        -- Map.Map k [v]
      kvs' = Map.toList kvs             -- [(k, [v])]
      vks = [[(vv, Set.singleton kk)] | (kk, vvs) <- kvs', vv <- vvs]  -- [[(v, Set.Set k)]]
      vks' = concat vks                 -- [(v, Set.Set k)]
  in Map.fromListWith Set.union vks'
      

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


-- extended euclidean algorithm: given a, b, return (g, s, t) such that g = gcd(a, b) and as + bt = g
euc :: Integer -> Integer -> (Integer, Integer, Integer)
euc a b = xeuc (1, a, 1, 0) (1, b, 0, 1)
  where
    xeuc (q0, r0, s0, t0) (q1, r1, s1, t1)
      | r1 == 0 = (r0, s0, t0)
      | otherwise = let (q, r) = divMod r0 r1
                    in  xeuc (q1, r1, s1, t1) (q, r, s0 - q * s1, t0 - q * t1)

-- Solve ax + by = c, giving ((s, t), (s', t')) where x = sr + t, y=s'r + t' is a solution for any r
solveDiophantine :: Integer -> Integer -> Integer -> ((Integer, Integer), (Integer, Integer))
solveDiophantine a b c =
  let (g, s, t) = euc a b                  -- find s and t such that as + bt = gcd(a,b)
      (x1, y1)  = (s * c `div` g, t * c `div` g)   -- find an example x1, y1 such that a.x1 + b.y1 = c
  in ((-b `div` g, x1), (a `div` g, y1))


{- If we have regular solutions for values of t:
    t = mx + f
    t = ny + g
   Then
    mx + f = ny + g, or
    mx + (-n)y = g - f
   then find a, b such that
    x = ar + b     (along with y = cr + d)
   satisfies both of these.
   Substitute in so that
    t = mx + f = mar + (mb + f)
   The result, (m', f') = (ma, mb + f)
-}

combineDiophantine (m, f) (n, g) =
  let ((a, b), _) = solveDiophantine m (-n) (g - f)
      m' = m * a
      f' = m * b + f
  in  (m', f')