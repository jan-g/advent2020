module Day24 where

import Data.Function ((&))
import Data.List.Split
import Data.List as L
import Data.Array as A
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe (catMaybes, isJust, fromJust)
import Text.ParserCombinators.ReadP as P
import Numeric (readInt)
import Data.Bits ((.&.), (.|.))

import Lib

{-
--- Day 24: Lobby Layout ---

Your raft makes it to the tropical island; it turns out that the small crab was an excellent navigator. You make your way to the resort.

As you enter the lobby, you discover a small problem: the floor is being renovated. You can't even reach the check-in desk until they've finished installing the new tile floor.

The tiles are all hexagonal; they need to be arranged in a hex grid with a very specific color pattern. Not in the mood to wait, you offer to help figure out the pattern.

The tiles are all white on one side and black on the other. They start with the white side facing up. The lobby is large enough to fit whatever pattern might need to appear there.

A member of the renovation crew gives you a list of the tiles that need to be flipped over (your puzzle input). Each line in the list identifies a single tile that needs to be flipped by giving a series of steps starting from a reference tile in the very center of the room. (Every line starts from the same reference tile.)

Because the tiles are hexagonal, every tile has six neighbors: east, southeast, southwest, west, northwest, and northeast. These directions are given in your list, respectively, as e, se, sw, w, nw, and ne. A tile is identified by a series of these directions with no delimiters; for example, esenee identifies the tile you land on if you start at the reference tile and then move one tile east, one tile southeast, one tile northeast, and one tile east.

Each time a tile is identified, it flips from white to black or from black to white. Tiles might be flipped more than once. For example, a line like esew flips a tile immediately adjacent to the reference tile, and a line like nwwswee flips the reference tile itself.

Here is a larger example:

sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew

In the above example, 10 tiles are flipped once (to black), and 5 more are flipped twice (to black, then back to white). After all of these instructions have been followed, a total of 10 tiles are black.

Go through the renovation crew's list and determine which tiles they need to flip. After all of the instructions have been followed, how many tiles are left with the black side up?

To begin, get your puzzle input.
-}

-- The grid looks like this:
{-
                 Z                                  Y
                  \                                /
                   \                              /




                            (-1, 1, 0) (0, 1, -1)
(-3, 0, 3) (-2, 0, 2) (-1, 0, 1)  (0, 0, 0)  (1, 0, -1) (2, 0, -2) (3, 0, -3)   --- x
                 (-1, -1, 2) (0, -1, 1) (1, -1, 0)
-}


type Coord = (Integer, Integer, Integer)

parse :: [String] -> [[Coord]]
parse ls = ls
         & map directions

directions "" = []
directions ('w':rest)     = (-1, 0, 1) : directions rest
directions ('e':rest)     = (1, 0, -1) : directions rest
directions ('n':'w':rest) = (-1, 1, 0) : directions rest
directions ('n':'e':rest) = (0, 1, -1) : directions rest
directions ('s':'w':rest) = (0, -1, 1) : directions rest
directions ('s':'e':rest) = (1, -1, 0) : directions rest

move :: Coord -> Coord -> Coord
move (x, y, z) (dx, dy, dz) = (x+dx, y+dy, z+dz)

run :: [Coord] -> Coord -> Coord
run moves from = foldl move from moves

type HexGrid = Set.Set Coord  -- sets of black tiles
flipHex :: Coord -> HexGrid -> HexGrid
flipHex at g = if Set.member at g then Set.delete at g else Set.insert at g

day24 ls =
  let moves = parse ls
      m0 = Set.empty
      final = foldl (\g ds -> flipHex (run ds (0, 0, 0)) g) m0 moves
  in Set.size final

{-
-}

day24b ls = "hello world"
