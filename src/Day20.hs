module Day20 where

import Data.Function ((&))
import Data.List.Split
import Data.List as L
import qualified Data.Array as A
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe (catMaybes, isJust, fromJust)
import Text.ParserCombinators.ReadP as P
import Numeric (readInt)
import Data.Bits ((.&.), (.|.))
import Control.Monad

import Lib

{-
--- Day 20: Jurassic Jigsaw ---

The high-speed train leaves the forest and quickly carries you south. You can even see a desert in the distance! Since you have some spare time, you might as well see if there was anything interesting in the image the Mythical Information Bureau satellite captured.

After decoding the satellite messages, you discover that the data actually contains many small images created by the satellite's camera array. The camera array consists of many cameras; rather than produce a single square image, they produce many smaller square image tiles that need to be reassembled back into a single image.

Each camera in the camera array returns a single monochrome image tile with a random unique ID number. The tiles (your puzzle input) arrived in a random order.

Worse yet, the camera array appears to be malfunctioning: each image tile has been rotated and flipped to a random orientation. Your first task is to reassemble the original image by orienting the tiles so they fit together.

To show how the tiles should be reassembled, each tile's image data includes a border that should line up exactly with its adjacent tiles. All tiles have this border, and the border lines up exactly when the tiles are both oriented correctly. Tiles at the edge of the image also have this border, but the outermost edges won't line up with any other tiles.

For example, suppose you have the following nine tiles:

Tile 2311:
..##.#..#.
##..#.....
#...##..#.
####.#...#
##.##.###.
##...#.###
.#.#.#..##
..#....#..
###...#.#.
..###..###

Tile 1951:
#.##...##.
#.####...#
.....#..##
#...######
.##.#....#
.###.#####
###.##.##.
.###....#.
..#.#..#.#
#...##.#..

Tile 1171:
####...##.
#..##.#..#
##.#..#.#.
.###.####.
..###.####
.##....##.
.#...####.
#.##.####.
####..#...
.....##...

Tile 1427:
###.##.#..
.#..#.##..
.#.##.#..#
#.#.#.##.#
....#...##
...##..##.
...#.#####
.#.####.#.
..#..###.#
..##.#..#.

Tile 1489:
##.#.#....
..##...#..
.##..##...
..#...#...
#####...#.
#..#.#.#.#
...#.#.#..
##.#...##.
..##.##.##
###.##.#..

Tile 2473:
#....####.
#..#.##...
#.##..#...
######.#.#
.#...#.#.#
.#########
.###.#..#.
########.#
##...##.#.
..###.#.#.

Tile 2971:
..#.#....#
#...###...
#.#.###...
##.##..#..
.#####..##
.#..####.#
#..#.#..#.
..####.###
..#.#.###.
...#.#.#.#

Tile 2729:
...#.#.#.#
####.#....
..#.#.....
....#..#.#
.##..##.#.
.#.####...
####.#.#..
##.####...
##..#.##..
#.##...##.

Tile 3079:
#.#.#####.
.#..######
..#.......
######....
####.#..#.
.#...#.##.
#.#####.##
..#.###...
..#.......
..#.###...

By rotating, flipping, and rearranging them, you can find a square arrangement that causes all adjacent borders to line up:

#...##.#.. ..###..### #.#.#####.
..#.#..#.# ###...#.#. .#..######
.###....#. ..#....#.. ..#.......
###.##.##. .#.#.#..## ######....
.###.##### ##...#.### ####.#..#.
.##.#....# ##.##.###. .#...#.##.
#...###### ####.#...# #.#####.##
.....#..## #...##..#. ..#.###...
#.####...# ##..#..... ..#.......
#.##...##. ..##.#..#. ..#.###...

#.##...##. ..##.#..#. ..#.###...
##..#.##.. ..#..###.# ##.##....#
##.####... .#.####.#. ..#.###..#
####.#.#.. ...#.##### ###.#..###
.#.####... ...##..##. .######.##
.##..##.#. ....#...## #.#.#.#...
....#..#.# #.#.#.##.# #.###.###.
..#.#..... .#.##.#..# #.###.##..
####.#.... .#..#.##.. .######...
...#.#.#.# ###.##.#.. .##...####

...#.#.#.# ###.##.#.. .##...####
..#.#.###. ..##.##.## #..#.##..#
..####.### ##.#...##. .#.#..#.##
#..#.#..#. ...#.#.#.. .####.###.
.#..####.# #..#.#.#.# ####.###..
.#####..## #####...#. .##....##.
##.##..#.. ..#...#... .####...#.
#.#.###... .##..##... .####.##.#
#...###... ..##...#.. ...#..####
..#.#....# ##.#.#.... ...##.....

For reference, the IDs of the above tiles are:

1951    2311    3079
2729    1427    2473
2971    1489    1171

To check that you've assembled the image correctly, multiply the IDs of the four corner tiles together. If you do this with the assembled tiles from the example above, you get 1951 * 3079 * 2971 * 1171 = 20899048083289.

Assemble the tiles into an image. What do you get if you multiply together the IDs of the four corner tiles?

To begin, get your puzzle input.
-}

parse ls = ls
         & splitOn [""]
         & filter (/= [])
         & map parseTile
         & Map.fromList

parseTile (head:rest) =
  let Just num = quickParse (string "Tile " *> natParser <* string ":") head
      m = loadMap rest
  in (num, m)

data Side = T | L | R | B deriving (Show, Eq)
data Flipped = Normal | Flipped deriving (Show, Eq)

bounds tile =
  let x0 = Map.keysSet tile & Set.map fst & minimum
      x1 = Map.keysSet tile & Set.map fst & maximum
      y0 = Map.keysSet tile & Set.map snd & minimum
      y1 = Map.keysSet tile & Set.map snd & maximum
  in (x0, x1, y0, y1)

e T tile = [tile Map.! (x, y0) | let (x0, x1, y0, y1) = bounds tile, x <- [x0..x1]]
e B tile = [tile Map.! (x, y1) | let (x0, x1, y0, y1) = bounds tile, x <- [x0..x1]]
e L tile = [tile Map.! (x0, y) | let (x0, x1, y0, y1) = bounds tile, y <- [y0..y1]]
e R tile = [tile Map.! (x1, y) | let (x0, x1, y0, y1) = bounds tile, y <- [y0..y1]]

opposite T = B
opposite B = T
opposite L = R
opposite R = L

rotLeft T = L
rotLeft B = R
rotLeft L = B
rotLeft R = T

rotRight = rotLeft . rotLeft . rotLeft

-- the orientation is whether the tile is flipped over, and which edge is at the top
-- the side is which side (relative to the new top) we're looking at
edge (Normal, T) tile side = e side tile
edge (Flipped, T) tile side
  | side == T || side == B = reverse $ e side tile
  | otherwise              = e (opposite side) tile
edge (Normal, L) tile side
  | side == T || side == B = reverse $ e (rotLeft side) tile
  | otherwise              = e (rotLeft side) tile
edge (Flipped, L) tile side
  | side == T || side == B = reverse $ edge (Normal, L) tile side
  | otherwise              = edge (Normal, L) tile (opposite side)
edge (Normal, R) tile side
  | side == T || side == B = e (rotRight side) tile
  | otherwise              = reverse $ e (rotRight side) tile
edge (Flipped, R) tile side
  | side == T || side == B = reverse $ edge (Normal, R) tile side
  | otherwise              = edge (Normal, R) tile (opposite side)
edge (Normal, B) tile side = reverse $ edge (Normal, T) tile (opposite side)
edge (Flipped, B) tile side = reverse $ edge (Flipped, T) tile (opposite side)

orientations = [(upside, top) | upside <- [Normal, Flipped], top <- [T, L, B, R]]


type Coord = (Int, Int)
type Tile = Map.Map Coord Char
type Orientation = (Flipped, Side)
type TileNum = Integer
search :: Coord
       -> Map.Map TileNum Tile
       -> Map.Map Coord (TileNum, Orientation, Tile)
       -> Coord
       -> Maybe (Map.Map Coord (TileNum, Orientation, Tile))

search (maxX, maxY) allTiles soFar (x, y) =
  if Map.size allTiles == 0
  then Just soFar
  else
  let possibles = do
        num <- Map.keys allTiles
        o <- possibleOrientations
        let tile = allTiles Map.! num
        -- this is not already chosen: we remove this tile from the remaining tiles
        guard $ fitsAbove tile o
        guard $ fitsRight tile o
        
        -- work out the remaining tiles
        let remaining = Map.delete num allTiles
            newGrid = Map.insert (x, y) (num, o, tile) soFar

        -- fit the remaining tiles
        let solution = search (maxX, maxY) remaining newGrid nextCoord
        guard $ isJust solution       
        return $ solution
  in if possibles == [] then Nothing else head possibles
  where
    possibleOrientations
      | (x, y) == (0, 0) = [(Normal, T)]
      | otherwise = orientations
    nextCoord
      | x == maxX - 1 = (0, y + 1)
      | otherwise     = (x + 1, y)
    fitsAbove t o
      | y == 0 = True
      | otherwise = let (aboveN, aboveO, aboveT) = soFar Map.! (x, y-1)
                    in  edge aboveO aboveT B == edge o t T
    fitsRight t o
      | x == 0 = True
      | otherwise = let (leftN, leftO, leftT) = soFar Map.! (x-1, y)
                    in edge leftO leftT R == edge o t L

day20 ls =
  let tiles = parse ls
      Just ans = Day20.search (12, 12) tiles Map.empty (0, 0)
      (tl, _, _) = ans Map.! (0, 0)
      (tr, _, _) = ans Map.! (11, 0)
      (bl, _, _) = ans Map.! (0, 11)
      (br, _, _) = ans Map.! (11, 11)
  in  tl * tr * bl * br

{-
-}

day20b ls = "hello world"
