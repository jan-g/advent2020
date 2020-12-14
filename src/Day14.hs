{-# LANGUAGE LambdaCase #-}

module Day14 where

import Data.Function ((&))
import Data.List.Split
import Data.Array
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe (catMaybes, isJust, fromJust)
import Text.ParserCombinators.ReadP as P
import Numeric (readInt)
import Data.Bits ((.&.), (.|.))

import Lib

{-
--- Day 14: Docking Data ---

As your ferry approaches the sea port, the captain asks for your help again. The computer system that runs this port isn't compatible with the docking program on the ferry, so the docking parameters aren't being correctly initialized in the docking program's memory.

After a brief inspection, you discover that the sea port's computer system uses a strange bitmask system in its initialization program. Although you don't have the correct decoder chip handy, you can emulate it in software!

The initialization program (your puzzle input) can either update the bitmask or write a value to memory. Values and memory addresses are both 36-bit unsigned integers. For example, ignoring bitmasks for a moment, a line like mem[8] = 11 would write the value 11 to memory address 8.

The bitmask is always given as a string of 36 bits, written with the most significant bit (representing 2^35) on the left and the least significant bit (2^0, that is, the 1s bit) on the right. The current bitmask is applied to values immediately before they are written to memory: a 0 or 1 overwrites the corresponding bit in the value, while an X leaves the bit in the value unchanged.

For example, consider the following program:

mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0

This program starts by specifying a bitmask (mask = ....). The mask it specifies will overwrite two bits in every written value: the 2s bit is overwritten with 0, and the 64s bit is overwritten with 1.

The program then attempts to write the value 11 to memory address 8. By expanding everything out to individual bits, the mask is applied as follows:

value:  000000000000000000000000000000001011  (decimal 11)
mask:   XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
result: 000000000000000000000000000001001001  (decimal 73)

So, because of the mask, the value 73 is written to memory address 8 instead. Then, the program tries to write 101 to address 7:

value:  000000000000000000000000000001100101  (decimal 101)
mask:   XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
result: 000000000000000000000000000001100101  (decimal 101)

This time, the mask has no effect, as the bits it overwrote were already the values the mask tried to set. Finally, the program tries to write 0 to address 8:

value:  000000000000000000000000000000000000  (decimal 0)
mask:   XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
result: 000000000000000000000000000001000000  (decimal 64)

64 is written to address 8 instead, overwriting the value that was there previously.

To initialize your ferry's docking program, you need the sum of all values left in memory after the initialization program completes. (The entire 36-bit address space begins initialized to the value 0 at every address.) In the above example, only two values in memory are not zero - 101 (at address 7) and 64 (at address 8) - producing a sum of 165.

Execute the initialization program. What is the sum of all values left in memory after it completes?

To begin, get your puzzle input.
-}

data Instr = Mask Integer Integer | Store Integer Integer
  deriving (Show, Eq)

parse ls = ls
         & map (quickParse parseLine)
         & catMaybes

parseLine :: ReadP Instr
parseLine = parseMask <++ parseStore

parseMask :: ReadP Instr
parseMask = do
  string "mask = "
  m <- many1 (satisfy $ not . isSpace)
  eof
  let and = readInt 2 isMask (\case '0' -> 0; _ -> 1) m & head & fst
      or = readInt 2 isMask (\case '1' -> 1; _ -> 0) m & head & fst
  return $ Mask and or
  where
    isMask '0' = True
    isMask '1' = True
    isMask 'X' = True
    isMask _ = False

parseStore :: ReadP Instr
parseStore = do
  string "mem["
  a <- natParser
  string "] = "
  v <- natParser
  eof
  return $ Store a v


data Memory = Memory Integer Integer (Map.Map Integer Integer)
  deriving (Show, Eq)

nullMemory :: Memory
nullMemory = Memory 0 0 Map.empty

apply :: Memory -> Instr -> Memory
apply (Memory _ _ m) (Mask a o)  = Memory a o m
apply (Memory a o m) (Store d v) = Memory a o (Map.insert d (v .&. a .|. o) m)

run :: Memory -> [Instr] -> Memory
run = foldl apply 

sumValues :: Memory -> Integer
sumValues (Memory a o m) = Map.toList m & map snd & sum

day14 ls =
  let is = parse ls
  in  run nullMemory is & sumValues

{-
-}

day14b ls = "hello world"
