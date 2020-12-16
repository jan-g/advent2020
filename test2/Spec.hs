import Test.Hspec
import Control.Exception (evaluate)

import Data.Array
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad
import Data.List.Split (splitOn)
import Data.List as L
import Data.Maybe (catMaybes)

import Lib
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day20
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24


main :: IO ()
main =
  hspec $ do
    describe "Day 16" $ do
      let example = "class: 1-3 or 5-7\n\
                    \row: 6-11 or 33-44\n\
                    \seat: 13-40 or 45-50\n\
                    \\n\
                    \your ticket:\n\
                    \7,1,14\n\
                    \\n\
                    \nearby tickets:\n\
                    \7,3,47\n\
                    \40,4,50\n\
                    \55,2,20\n\
                    \38,6,12" & lines
          (cs, me, them) = Day16.parse example
      it "validates a field entry" $ do
        Map.size (Day16.anyMatches cs 7) `shouldBe` 2
      it "validates the first ticket" $ do
        Day16.anyMatchAllFields cs [7, 3, 47] `shouldBe` True
      it "locates valid and invalid tickets" $ do
        let (valid, invalid) = Day16.initialValidation cs them
        length invalid `shouldBe` 3
        valid `shouldBe` [[7, 3, 47]]
      it "locates invalid fields" $ do
        Day16.allInvalids cs them `shouldBe` [4, 55, 12]

      let example2 = "class: 0-1 or 4-19\n\
                     \row: 0-5 or 8-19\n\
                     \seat: 0-13 or 16-19\n\
                     \\n\
                     \your ticket:\n\
                     \11,12,13\n\
                     \\n\
                     \nearby tickets:\n\
                     \3,9,18\n\
                     \15,1,5\n\
                     \5,14,9" & lines
          (cs, me, them) = Day16.parse example2
      it "parses correctly" $ do
        them `shouldBe` [[3, 9, 18], [15, 1, 5], [5, 14, 9]]
      it "works out the things each field could be" $ do
        let fs = Day16.dictToSet cs
        Set.size (Day16.candidatesForFieldValues fs [3, 15, 5]) `shouldBe` 1  -- row
        Set.size (Day16.candidatesForFieldValues fs [9, 1, 14]) `shouldBe` 2  -- class row
        Set.size (Day16.candidatesForFieldValues fs [18, 5, 9]) `shouldBe` 3  -- class row seat

      it "works out the whole set of candidates" $ do
        let fs = Day16.dictToSet cs
        (Day16.candidatesForTickets fs them & map (Set.map Day16.fieldName)) `shouldBe` [ Set.fromList ["row"]
                                                                                        , Set.fromList ["class", "row"]
                                                                                        , Set.fromList ["class", "row", "seat"]]
      it "winnows the set of candidates" $ do
        let fs = Day16.dictToSet cs
            cands = Day16.candidatesForTickets fs them
        (Day16.winnowCandidateSets cands & map (Set.map Day16.fieldName)) `shouldBe` [ Set.fromList ["row"]
                                                                                     , Set.fromList ["class"]
                                                                                     , Set.fromList ["seat"]]
