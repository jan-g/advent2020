module Day17 where

import Data.Function ((&))
import Data.List.Split
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe (catMaybes, isJust, fromJust)
import Text.ParserCombinators.ReadP as P
import Numeric (readInt)
import Data.Bits ((.&.), (.|.))

import Lib

{-
--- Day 17: Conway Cubes ---

As your flight slowly drifts through the sky, the Elves at the Mythical Information Bureau at the North Pole contact you. They'd like some help debugging a malfunctioning experimental energy source aboard one of their super-secret imaging satellites.

The experimental energy source is based on cutting-edge technology: a set of Conway Cubes contained in a pocket dimension! When you hear it's having problems, you can't help but agree to take a look.

The pocket dimension contains an infinite 3-dimensional grid. At every integer 3-dimensional coordinate (x,y,z), there exists a single cube which is either active or inactive.

In the initial state of the pocket dimension, almost all cubes start inactive. The only exception to this is a small flat region of cubes (your puzzle input); the cubes in this region start in the specified active (#) or inactive (.) state.

The energy source then proceeds to boot up by executing six cycles.

Each cube only ever considers its neighbors: any of the 26 other cubes where any of their coordinates differ by at most 1. For example, given the cube at x=1,y=2,z=3, its neighbors include the cube at x=2,y=2,z=2, the cube at x=0,y=2,z=3, and so on.

During a cycle, all cubes simultaneously change their state according to the following rules:

    If a cube is active and exactly 2 or 3 of its neighbors are also active, the cube remains active. Otherwise, the cube becomes inactive.
    If a cube is inactive but exactly 3 of its neighbors are active, the cube becomes active. Otherwise, the cube remains inactive.

The engineers responsible for this experimental energy source would like you to simulate the pocket dimension and determine what the configuration of cubes should be at the end of the six-cycle boot process.

For example, consider the following initial state:

.#.
..#
###

Even though the pocket dimension is 3-dimensional, this initial state represents a small 2-dimensional slice of it. (In particular, this initial state defines a 3x3x1 region of the 3-dimensional space.)

Simulating a few cycles from this initial state produces the following configurations, where the result of each cycle is shown layer-by-layer at each given z coordinate (and the frame of view follows the active cells in each cycle):

Before any cycles:

z=0
.#.
..#
###


After 1 cycle:

z=-1
#..
..#
.#.

z=0
#.#
.##
.#.

z=1
#..
..#
.#.


After 2 cycles:

z=-2
.....
.....
..#..
.....
.....

z=-1
..#..
.#..#
....#
.#...
.....

z=0
##...
##...
#....
....#
.###.

z=1
..#..
.#..#
....#
.#...
.....

z=2
.....
.....
..#..
.....
.....


After 3 cycles:

z=-2
.......
.......
..##...
..###..
.......
.......
.......

z=-1
..#....
...#...
#......
.....##
.#...#.
..#.#..
...#...

z=0
...#...
.......
#......
.......
.....##
.##.#..
...#...

z=1
..#....
...#...
#......
.....##
.#...#.
..#.#..
...#...

z=2
.......
.......
..##...
..###..
.......
.......
.......

After the full six-cycle boot process completes, 112 cubes are left in the active state.

Starting with your given initial configuration, simulate six cycles. How many cubes are left in the active state after the sixth cycle?

To begin, get your puzzle input.
-}

type Coord = (Integer, Integer, Integer)
type Grid = Set.Set Coord

parse :: [String] -> Grid
parse ls =
  let flat = loadMap ls & Map.filter (=='#')
  in  Set.fromList (Map.toList flat & map (\((x,y), _) -> (x,y,0)))

at :: Grid -> Coord -> Bool
at g (x, y, z) = Set.member (x, y, z) g

x (a,b,c) = a
y (a,b,c) = b
z (a,b,c) = c


bounds g =
  let xs = Set.map x g
      ys = Set.map y g
      zs = Set.map z g
  in ((minimum xs, maximum xs), (minimum ys, maximum ys), (minimum zs, maximum zs))


step g =
  let ((x0, x1), (y0, y1), (z0, z1)) = bounds g
  in  Set.fromList [ (x, y, z) | x <- [x0-1..x1+1], y <- [y0-1..y1+1], z <- [z0-1..z1+1],
                     let c = count (x,y,z),
                     case g `at` (x,y,z) of
                       True -> c == 3 || c == 4
                       False -> c == 3
                   ]       
  where
    count (x,y,z) = length [True | dx <- [-1..1], dy <- [-1..1], dz <- [-1..1], g `at` (x+dx, y+dy, z+dz)]


normalise g =
  -- Only adjust x and y coordinates
  let ((x0, x1), (y0, y1), (z0, z1)) = bounds g
  in  Set.map (\(x,y,z) -> (x-x0, y-y0, z)) g

multiload :: [String] -> Grid
multiload ss =
  let s3 = splitOn [""] ss :: [[String]]
      height = (length s3 - 1) `div` 2
  in  Set.unions [zshift z (parse s) | (z,s) <- [-height..height] `zip` s3]
  where
    zshift d m = Set.map (\(x,y,z) -> (x,y,z+fromIntegral d)) m


day17 ls =
  let g0 = parse ls
  in  (iterate step g0) !! 6 & Set.size

{-
--- Part Two ---

For some reason, your simulated results don't match what the experimental energy source engineers expected. Apparently, the pocket dimension actually has four spatial dimensions, not three.

The pocket dimension contains an infinite 4-dimensional grid. At every integer 4-dimensional coordinate (x,y,z,w), there exists a single cube (really, a hypercube) which is still either active or inactive.

Each cube only ever considers its neighbors: any of the 80 other cubes where any of their coordinates differ by at most 1. For example, given the cube at x=1,y=2,z=3,w=4, its neighbors include the cube at x=2,y=2,z=3,w=3, the cube at x=0,y=2,z=3,w=4, and so on.

The initial state of the pocket dimension still consists of a small flat region of cubes. Furthermore, the same rules for cycle updating still apply: during each cycle, consider the number of active neighbors of each cube.

For example, consider the same initial state as in the example above. Even though the pocket dimension is 4-dimensional, this initial state represents a small 2-dimensional slice of it. (In particular, this initial state defines a 3x3x1x1 region of the 4-dimensional space.)

Simulating a few cycles from this initial state produces the following configurations, where the result of each cycle is shown layer-by-layer at each given z and w coordinate:

Before any cycles:

z=0, w=0
.#.
..#
###


After 1 cycle:

z=-1, w=-1
#..
..#
.#.

z=0, w=-1
#..
..#
.#.

z=1, w=-1
#..
..#
.#.

z=-1, w=0
#..
..#
.#.

z=0, w=0
#.#
.##
.#.

z=1, w=0
#..
..#
.#.

z=-1, w=1
#..
..#
.#.

z=0, w=1
#..
..#
.#.

z=1, w=1
#..
..#
.#.


After 2 cycles:

z=-2, w=-2
.....
.....
..#..
.....
.....

z=-1, w=-2
.....
.....
.....
.....
.....

z=0, w=-2
###..
##.##
#...#
.#..#
.###.

z=1, w=-2
.....
.....
.....
.....
.....

z=2, w=-2
.....
.....
..#..
.....
.....

z=-2, w=-1
.....
.....
.....
.....
.....

z=-1, w=-1
.....
.....
.....
.....
.....

z=0, w=-1
.....
.....
.....
.....
.....

z=1, w=-1
.....
.....
.....
.....
.....

z=2, w=-1
.....
.....
.....
.....
.....

z=-2, w=0
###..
##.##
#...#
.#..#
.###.

z=-1, w=0
.....
.....
.....
.....
.....

z=0, w=0
.....
.....
.....
.....
.....

z=1, w=0
.....
.....
.....
.....
.....

z=2, w=0
###..
##.##
#...#
.#..#
.###.

z=-2, w=1
.....
.....
.....
.....
.....

z=-1, w=1
.....
.....
.....
.....
.....

z=0, w=1
.....
.....
.....
.....
.....

z=1, w=1
.....
.....
.....
.....
.....

z=2, w=1
.....
.....
.....
.....
.....

z=-2, w=2
.....
.....
..#..
.....
.....

z=-1, w=2
.....
.....
.....
.....
.....

z=0, w=2
###..
##.##
#...#
.#..#
.###.

z=1, w=2
.....
.....
.....
.....
.....

z=2, w=2
.....
.....
..#..
.....
.....

After the full six-cycle boot process completes, 848 cubes are left in the active state.

Starting with your given initial configuration, simulate six cycles in a 4-dimensional space. How many cubes are left in the active state after the sixth cycle?
-}

type Coord4 = (Integer, Integer, Integer, Integer)
type Grid4 = Set.Set Coord4

parse' :: [String] -> Grid4
parse' ls =
  let flat = loadMap ls & Map.filter (=='#')
  in  Set.fromList (Map.toList flat & map (\((x,y), _) -> (x,y,0,0)))

at4 :: Grid4 -> Coord4 -> Bool
at4 g (x, y, z, w) = Set.member (x, y, z, w) g

x4 (a,b,c,d) = a
y4 (a,b,c,d) = b
z4 (a,b,c,d) = c
w4 (a,b,c,d) = d


bounds4 g =
  let xs = Set.map x4 g
      ys = Set.map y4 g
      zs = Set.map z4 g
      ws = Set.map w4 g
  in ((minimum xs, maximum xs), (minimum ys, maximum ys), (minimum zs, maximum zs), (minimum ws, maximum ws))


step4 :: Grid4 -> Grid4
step4 g =
  let ((x0, x1), (y0, y1), (z0, z1), (w0, w1)) = bounds4 g
  in  Set.fromList [ (x, y, z, w) | x <- [x0-1..x1+1], y <- [y0-1..y1+1], z <- [z0-1..z1+1], w <- [w0-1..w1+1],
                     let c = count (x,y,z,w),
                     case g `at4` (x,y,z,w) of
                       True -> c == 3 || c == 4
                       False -> c == 3
                   ]       
  where
    count (x,y,z,w) = length [True | dx <- [-1..1], dy <- [-1..1], dz <- [-1..1], dw<-[-1..1], g `at4` (x+dx, y+dy, z+dz, w+dw)]


day17b ls =
  let g0 = parse' ls
  in  (iterate step4 g0) !! 6 & Set.size
