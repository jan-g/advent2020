module Day21 where

import Data.Function ((&))
import Text.ParserCombinators.ReadP as P
import Data.List as L
import Data.Array as A
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Char
import Data.Maybe (catMaybes, isJust, fromJust)
import Numeric (readInt)
import Data.Bits ((.&.), (.|.))

import Lib

{-
--- Day 21: Allergen Assessment ---

You reach the train's last stop and the closest you can get to your vacation island without getting wet. There aren't even any boats here, but nothing can stop you now: you build a raft. You just need a few days' worth of food for your journey.

You don't speak the local language, so you can't read any ingredients lists. However, sometimes, allergens are listed in a language you do understand. You should be able to use this information to determine which ingredient contains which allergen and work out which foods are safe to take with you on your trip.

You start by compiling a list of foods (your puzzle input), one food per line. Each line includes that food's ingredients list followed by some or all of the allergens the food contains.

Each allergen is found in exactly one ingredient. Each ingredient contains zero or one allergen. Allergens aren't always marked; when they're listed (as in (contains nuts, shellfish) after an ingredients list), the ingredient that contains each listed allergen will be somewhere in the corresponding ingredients list. However, even if an allergen isn't listed, the ingredient that contains that allergen could still be present: maybe they forgot to label it, or maybe it was labeled in a language you don't know.

For example, consider the following list of foods:

mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)

The first food in the list has four ingredients (written in a language you don't understand): mxmxvkd, kfcds, sqjhc, and nhms. While the food might contain other allergens, a few allergens the food definitely contains are listed afterward: dairy and fish.

The first step is to determine which ingredients can't possibly contain any of the allergens in any food in your list. In the above example, none of the ingredients kfcds, nhms, sbzzf, or trh can contain an allergen. Counting the number of times any of these ingredients appear in any ingredients list produces 5: they all appear once each except sbzzf, which appears twice.

Determine which ingredients cannot possibly contain any of the allergens in your list. How many times do any of those ingredients appear?

To begin, get your puzzle input.
-}


parse ls = ls
         & map (quickParse parseIngredient)
         & catMaybes

parseIngredient :: ReadP (Set.Set String, Set.Set String)
parseIngredient = do
  ing <- sepBy (many1 (satisfy isAlpha)) (char ' ')
  string " (contains "
  all <- sepBy (many1 (satisfy isAlpha)) (string ", ")
  string ")"
  eof
  return (Set.fromList ing, Set.fromList all)

-- narrow a set of ingredients by looking at the
narrow :: Map.Map String (Set.Set String) -> [(Set.Set String, Set.Set String)] -> Map.Map String (Set.Set String)
narrow possibles [] = possibles
narrow possibles ((ing, att):rest) =
  let possibles' = foldr (narrow1 ing) possibles att
  in narrow possibles' rest
  where
    narrow1 :: Set.Set String -> String -> Map.Map String (Set.Set String) -> Map.Map String (Set.Set String)
    narrow1 options a ps =
      Map.update (\items -> Just (Set.intersection items options)) a ps

day21 ls =
  let rules = parse ls
      allergens = map snd rules & Set.unions
      ingredients = map fst rules & Set.unions
      possibles = Map.fromList [(a, ingredients) | a <- Set.toList allergens]
      possibles' = narrow possibles rules
      used = Map.toList possibles' & map snd & Set.unions
      unused = ingredients `Set.difference` used
      appearances = rules & map fst & map (Set.intersection unused) & map Set.size & sum
  in appearances

{-
--- Part Two ---

Now that you've isolated the inert ingredients, you should have enough information to figure out which ingredient contains which allergen.

In the above example:

    mxmxvkd contains dairy.
    sqjhc contains fish.
    fvjkl contains soy.

Arrange the ingredients alphabetically by their allergen and separate them by commas to produce your canonical dangerous ingredient list. (There should not be any spaces in your canonical dangerous ingredient list.) In the above example, this would be mxmxvkd,sqjhc,fvjkl.

Time to stock your raft with supplies. What is your canonical dangerous ingredient list?
-}



day21b ls =
  let rules = parse ls
      allergens = map snd rules & Set.unions
      ingredients = map fst rules & Set.unions
      possibles = Map.fromList [(a, ingredients) | a <- Set.toList allergens]
      possibles' = narrow possibles rules
      Right solution = narrowCandidateMap possibles'
  in solution & Map.toList & map snd & intercalate ","

