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
import qualified Text.ParserCombinators.ReadP as P

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

    describe "Day 17" $ do
      let example = ".#.\n\
                    \..#\n\
                    \###" & lines
          g0 = Day17.parse example
      it "parses the grid correctly" $ do
        g0 `shouldBe` Set.fromList [(1,0,0), (2,1,0), (0,2,0), (1,2,0), (2,2,0)]
      it "calculates bounds correctly" $ do
        Day17.bounds g0 `shouldBe` ((0,2), (0,2), (0,0))

      let s1 = "#..\n\
               \..#\n\
               \.#.\n\
               \\n\
               \#.#\n\
               \.##\n\
               \.#.\n\
               \\n\
               \#..\n\
               \..#\n\
               \.#." & lines
          g1 = Day17.multiload s1
          expected = Set.fromList [(0,0,-1), (2,1,-1), (1,2,-1),
                                   (0,0,0), (2,0,0), (1,1,0), (2,1,0), (1,2,0),
                                   (0,0,1), (2,1,1), (1,2,1)]
      it "multiloads" $ do
        g1 `shouldBe` expected
      it "computes the next step correctly" $ do
        let next = Day17.step g0
        Day17.normalise next `shouldBe` expected

      it "computes three cycles correctly" $ do
        let s3 = ".......\n\
                 \.......\n\
                 \..##...\n\
                 \..###..\n\
                 \.......\n\
                 \.......\n\
                 \.......\n\
                 \\n\
                 \..#....\n\
                 \...#...\n\
                 \#......\n\
                 \.....##\n\
                 \.#...#.\n\
                 \..#.#..\n\
                 \...#...\n\
                 \\n\
                 \...#...\n\
                 \.......\n\
                 \#......\n\
                 \.......\n\
                 \.....##\n\
                 \.##.#..\n\
                 \...#...\n\
                 \\n\
                 \..#....\n\
                 \...#...\n\
                 \#......\n\
                 \.....##\n\
                 \.#...#.\n\
                 \..#.#..\n\
                 \...#...\n\
                 \\n\
                 \.......\n\
                 \.......\n\
                 \..##...\n\
                 \..###..\n\
                 \.......\n\
                 \.......\n\
                 \......." & lines
            g3 = Day17.multiload s3
        ((iterate Day17.step g0) !! 3 & Day17.normalise) `shouldBe` g3

      it "solves part A for the example" $ do
        Day17.day17 example `shouldBe` 112

      it "solves part b for the example" $ do
        Day17.day17b example `shouldBe` 848

    describe "Day 18" $ do
      it "Parses expressions" $ do
        Day18.eval "1" `shouldBe` Just 1
        Day18.eval "1+1" `shouldBe` Just 2
        Day18.eval " 1 " `shouldBe` Just 1
        Day18.eval " 1 +  1  " `shouldBe` Just 2
      it "parses operators" $ do
        let Just o = quickParse Day18.op " +"
        1 `o` 9 `shouldBe` 10
      it "parses otehr expressions" $ do
        Day18.eval "1 + 2 * 3 + 4 * 5 + 6" `shouldBe` Just 71
      it "parses operators" $ do
        let Just m = quickParse Day18.mult " * "
        let Just p = quickParse Day18.plus " + "
        2 `m` 3 `shouldBe` 6
        2 `p` 3 `shouldBe` 5

      forM_ [ ("1 + 2 * 3 + 4 * 5 + 6", 231)
            , ("1 + (2 * 3) + (4 * (5 + 6))", 51)
            , ("2 * 3 + (4 * 5)", 46)
            , ("5 + (8 * 3 + 9 + 3 * 4 * 3)", 1445)
            , ("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))", 669060)
            , ("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2", 23340)
            ] $ \(e, v) -> do
        it ("parses " ++ e) $ do
          Day18.eval' e `shouldBe` Just v

    describe "day 19" $ do
      let exampleRules = "0: 4 1 5\n\
                         \1: 2 3 | 3 2\n\
                         \2: 4 4 | 5 5\n\
                         \3: 4 5 | 5 4\n\
                         \4: \"a\"\n\
                         \5: \"b\"" & lines
          examplePatterns = "ababbb\n\
                            \bababa\n\
                            \abbbab\n\
                            \aaabbb\n\
                            \aaaabbb" & lines
          rule = Day19.parseRules exampleRules
      it "should compile the rule" $ do
        let r4 = Day19.Match 'a'
            r5 = Day19.Match 'b'
            r2 = Day19.Alt [Day19.Seq [r4, r4], Day19.Seq [r5, r5]]
            r3 = Day19.Alt [Day19.Seq [r4, r5], Day19.Seq [r5, r4]]
            r1 = Day19.Alt [Day19.Seq [r2, r3], Day19.Seq [r3, r2]]
            r0 = Day19.Seq [r4, r1, r5]
        rule `shouldBe` r0

      it "should match correctly" $ do
        Day19.matches rule "ababbb" `shouldBe` True
        Day19.matches rule "abbbab" `shouldBe` True

      it "should fail to match correctly" $ do
        Day19.matches rule "bababa" `shouldBe` False
        Day19.matches rule "aaabbb" `shouldBe` False
        Day19.matches rule "aaaabbb" `shouldBe` False

      it "filters correctly" $ do
        Day19.day19 (exampleRules ++ [""] ++ examplePatterns) `shouldBe` 2
