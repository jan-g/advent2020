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
import qualified Data.Sequence as Seq

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
import qualified Day25


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

      it "repeats single strings" $ do
        Day19.repeatString 5 " " "42" `shouldBe` "42 42 42 42 42"
        take 3 (Day19.repeatsOfString " " "42") `shouldBe` ["42", "42 42", "42 42 42"]

    describe "Day 20" $ do
      let t = "Tile 1:\n\
              \abc\n\
              \def\n\
              \ghi" & lines
          (1, tile) = Day20.parseTile t
      forM_ [ ((Day20.Normal, Day20.T), Day20.T, "abc")
            , ((Day20.Normal, Day20.T), Day20.B, "ghi")
            , ((Day20.Normal, Day20.T), Day20.L, "adg")
            , ((Day20.Normal, Day20.T), Day20.R, "cfi")

            , ((Day20.Normal, Day20.L), Day20.T, "gda")
            , ((Day20.Normal, Day20.L), Day20.B, "ifc")
            , ((Day20.Normal, Day20.L), Day20.L, "ghi")
            , ((Day20.Normal, Day20.L), Day20.R, "abc")

            , ((Day20.Normal, Day20.B), Day20.T, "ihg")
            , ((Day20.Normal, Day20.B), Day20.B, "cba")
            , ((Day20.Normal, Day20.B), Day20.L, "ifc")
            , ((Day20.Normal, Day20.B), Day20.R, "gda")

            , ((Day20.Normal, Day20.R), Day20.T, "cfi")
            , ((Day20.Normal, Day20.R), Day20.B, "adg")
            , ((Day20.Normal, Day20.R), Day20.L, "cba")
            , ((Day20.Normal, Day20.R), Day20.R, "ihg")

            , ((Day20.Flipped, Day20.T), Day20.T, "cba")
            , ((Day20.Flipped, Day20.T), Day20.B, "ihg")
            , ((Day20.Flipped, Day20.T), Day20.L, "cfi")
            , ((Day20.Flipped, Day20.T), Day20.R, "adg")

            , ((Day20.Flipped, Day20.L), Day20.T, "adg")
            , ((Day20.Flipped, Day20.L), Day20.B, "cfi")
            , ((Day20.Flipped, Day20.L), Day20.L, "abc")
            , ((Day20.Flipped, Day20.L), Day20.R, "ghi")

            , ((Day20.Flipped, Day20.B), Day20.T, "ghi")
            , ((Day20.Flipped, Day20.B), Day20.B, "abc")
            , ((Day20.Flipped, Day20.B), Day20.L, "gda")
            , ((Day20.Flipped, Day20.B), Day20.R, "ifc")

            , ((Day20.Flipped, Day20.R), Day20.T, "ifc")
            , ((Day20.Flipped, Day20.R), Day20.B, "gda")
            , ((Day20.Flipped, Day20.R), Day20.L, "ihg")
            , ((Day20.Flipped, Day20.R), Day20.R, "cba")

            ] $ \(orient, side, value) -> do
        it ("orientation:" ++ (show orient) ++ " side=" ++ (show side)) $ do
          Day20.edge orient tile side `shouldBe` value

      let example = "Tile 2311:\n\
                    \..##.#..#.\n\
                    \##..#.....\n\
                    \#...##..#.\n\
                    \####.#...#\n\
                    \##.##.###.\n\
                    \##...#.###\n\
                    \.#.#.#..##\n\
                    \..#....#..\n\
                    \###...#.#.\n\
                    \..###..###\n\
                    \\n\
                    \Tile 1951:\n\
                    \#.##...##.\n\
                    \#.####...#\n\
                    \.....#..##\n\
                    \#...######\n\
                    \.##.#....#\n\
                    \.###.#####\n\
                    \###.##.##.\n\
                    \.###....#.\n\
                    \..#.#..#.#\n\
                    \#...##.#..\n\
                    \\n\
                    \Tile 1171:\n\
                    \####...##.\n\
                    \#..##.#..#\n\
                    \##.#..#.#.\n\
                    \.###.####.\n\
                    \..###.####\n\
                    \.##....##.\n\
                    \.#...####.\n\
                    \#.##.####.\n\
                    \####..#...\n\
                    \.....##...\n\
                    \\n\
                    \Tile 1427:\n\
                    \###.##.#..\n\
                    \.#..#.##..\n\
                    \.#.##.#..#\n\
                    \#.#.#.##.#\n\
                    \....#...##\n\
                    \...##..##.\n\
                    \...#.#####\n\
                    \.#.####.#.\n\
                    \..#..###.#\n\
                    \..##.#..#.\n\
                    \\n\
                    \Tile 1489:\n\
                    \##.#.#....\n\
                    \..##...#..\n\
                    \.##..##...\n\
                    \..#...#...\n\
                    \#####...#.\n\
                    \#..#.#.#.#\n\
                    \...#.#.#..\n\
                    \##.#...##.\n\
                    \..##.##.##\n\
                    \###.##.#..\n\
                    \\n\
                    \Tile 2473:\n\
                    \#....####.\n\
                    \#..#.##...\n\
                    \#.##..#...\n\
                    \######.#.#\n\
                    \.#...#.#.#\n\
                    \.#########\n\
                    \.###.#..#.\n\
                    \########.#\n\
                    \##...##.#.\n\
                    \..###.#.#.\n\
                    \\n\
                    \Tile 2971:\n\
                    \..#.#....#\n\
                    \#...###...\n\
                    \#.#.###...\n\
                    \##.##..#..\n\
                    \.#####..##\n\
                    \.#..####.#\n\
                    \#..#.#..#.\n\
                    \..####.###\n\
                    \..#.#.###.\n\
                    \...#.#.#.#\n\
                    \\n\
                    \Tile 2729:\n\
                    \...#.#.#.#\n\
                    \####.#....\n\
                    \..#.#.....\n\
                    \....#..#.#\n\
                    \.##..##.#.\n\
                    \.#.####...\n\
                    \####.#.#..\n\
                    \##.####...\n\
                    \##..#.##..\n\
                    \#.##...##.\n\
                    \\n\
                    \Tile 3079:\n\
                    \#.#.#####.\n\
                    \.#..######\n\
                    \..#.......\n\
                    \######....\n\
                    \####.#..#.\n\
                    \.#...#.##.\n\
                    \#.#####.##\n\
                    \..#.###...\n\
                    \..#.......\n\
                    \..#.###..." & lines

      it "searches more regularly for a solution" $ do
        let allTiles = Day20.parse example
        let Just ans = Day20.search (3, 3) allTiles Map.empty (0, 0)
            (tl, _, _) = ans Map.! (0, 0)
            (tr, _, _) = ans Map.! (2, 0)
            (bl, _, _) = ans Map.! (0, 2)
            (br, _, _) = ans Map.! (2, 2)
        tl * tr * bl * br `shouldBe` 1951 * 3079 * 2971 * 1171

      forM_ [ ((Day20.Normal, Day20.T), "abc\ndef\nghi")
            , ((Day20.Normal, Day20.L), "gda\nheb\nifc")
            , ((Day20.Normal, Day20.B), "ihg\nfed\ncba")
            , ((Day20.Normal, Day20.R), "cfi\nbeh\nadg")
            , ((Day20.Flipped, Day20.T), "cba\nfed\nihg")
            , ((Day20.Flipped, Day20.L), "adg\nbeh\ncfi")
            , ((Day20.Flipped, Day20.B), "ghi\ndef\nabc")
            , ((Day20.Flipped, Day20.R), "ifc\nheb\ngda")
            ] $ \(o, v) -> do

        it ("correctly reorients tiles at " ++ (show o)) $ do
          Day20.orientTile o tile `shouldBe` loadMap (lines v)

      let expected = ".#.#..#.##...#.##..#####\n\
                     \###....#.#....#..#......\n\
                     \##.##.###.#.#..######...\n\
                     \###.#####...#.#####.#..#\n\
                     \##.#....#.##.####...#.##\n\
                     \...########.#....#####.#\n\
                     \....#..#...##..#.#.###..\n\
                     \.####...#..#.....#......\n\
                     \#..#.##..#..###.#.##....\n\
                     \#.####..#.####.#.#.###..\n\
                     \###.#.#...#.######.#..##\n\
                     \#.####....##..########.#\n\
                     \##..##.#...#...#.#.#.#..\n\
                     \...#..#..#.#.##..###.###\n\
                     \.#.#....#.##.#...###.##.\n\
                     \###.#...#..#.##.######..\n\
                     \.#.#.###.##.##.#..#.##..\n\
                     \.####.###.#...###.#..#.#\n\
                     \..#.#..#..#.#.#.####.###\n\
                     \#..####...#.#.#.###.###.\n\
                     \#####..#####...###....##\n\
                     \#.##..#..#...#..####...#\n\
                     \.#.###..##..##..####.##.\n\
                     \...###...##...#...#..###" & lines
          lookingFor = loadMap expected :: Map.Map (Int, Int) Char
      it "correctly assembles the example picture" $ do
        let allTiles = Day20.parse example
        let Just ans = Day20.search (3, 3) allTiles Map.empty (0, 0)
            assembled = Day20.assemblePicture ans
            possibles = (iterate rotateLeftMap lookingFor & take 4) ++ (iterate rotateLeftMap (flipXMap lookingFor) & take 4)
        assembled `elem` possibles `shouldBe` True

      it "trivially locates a sea monster" $ do
        let monsterPic = "                  # \n\
                         \#    ##    ##    ###\n\
                         \ #  #  #  #  #  #   " & lines & loadMap :: Map.Map (Int, Int) Char
        Day20.monsterAt (0, 0) monsterPic `shouldBe` True

      it "locates positions of sea monsters" $ do
        (map Day20.findSeaMonsters (Day20.allOrientations lookingFor) & Set.fromList) `shouldBe` Set.fromList [Set.empty, Set.fromList [(2, 2), (1, 16)]]

    describe "Day 21" $ do
      let example = "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\n\
                    \trh fvjkl sbzzf mxmxvkd (contains dairy)\n\
                    \sqjhc fvjkl (contains soy)\n\
                    \sqjhc mxmxvkd sbzzf (contains fish)" & lines
      it "solves the example for part 1" $ do
        Day21.day21 example `shouldBe` 5

      it "solves the example for part 2" $ do
        let rules = Day21.parse example
            allergens = map snd rules & Set.unions
            ingredients = map fst rules & Set.unions
            possibles = Map.fromList [(a, ingredients) | a <- Set.toList allergens]
            possibles' = Day21.narrow possibles rules
        narrowCandidateMap possibles' `shouldBe` Right (Map.fromList [ ("dairy", "mxmxvkd")
                                                                           , ("fish", "sqjhc")
                                                                           , ("soy", "fvjkl")
                                                                           ])
      it "produces the final answer for part 2" $ do
        Day21.day21b example `shouldBe` "mxmxvkd,sqjhc,fvjkl"

    describe "Day 22" $ do
      let example ="Player 1:\n\
                   \9\n\
                   \2\n\
                   \6\n\
                   \3\n\
                   \1\n\
                   \\n\
                   \Player 2:\n\
                   \5\n\
                   \8\n\
                   \4\n\
                   \7\n\
                   \10" & lines
      it "solves the example" $ do
        let [a, b] = Day22.parse example
        Day22.result (a, b) `shouldBe` ([], [3, 2, 10, 6, 8, 5, 9, 4, 7, 1])
      it "scores a winning hand" $ do
        Day22.score [3, 2, 10, 6, 8, 5, 9, 4, 7, 1] `shouldBe` 306

      it "runs the example game" $ do
        let [a, b] = Day22.parse example
            result = Day22.play' (Seq.fromList a, Seq.fromList b) Set.empty
        result `shouldBe` Right (Seq.fromList [7, 5, 6, 2, 4, 1, 10, 8, 9, 3])
      it "scores the winning hand" $ do
        Day22.day22b example `shouldBe` 291

    describe "Day 23" $ do
      it "computes next moves" $ do
        Day23.next "389125467" `shouldBe` "289154673"
        Day23.next "289154673" `shouldBe` "546789132"
        Day23.next "546789132" `shouldBe` "891346725"
        Day23.next "891346725" `shouldBe` "467913258"
        Day23.next "467913258" `shouldBe` "136792584"
    
      it "iterates successfully" $ do
        (iterate Day23.next "389125467" & drop 10 & head) `shouldBe` "837419265"
      
      it "summarises the cups" $ do
        (iterate Day23.next "389125467" & drop 10 & head & Day23.summarise '1') `shouldBe` "92658374"

      it "runs the example" $ do
        Day23.day23 ["389125467"] `shouldBe` "67384529"    

    describe "day 24" $ do
      let example = "sesenwnenenewseeswwswswwnenewsewsw\n\
                    \neeenesenwnwwswnenewnwwsewnenwseswesw\n\
                    \seswneswswsenwwnwse\n\
                    \nwnwneseeswswnenewneswwnewseswneseene\n\
                    \swweswneswnenwsewnwneneseenw\n\
                    \eesenwseswswnenwswnwnwsewwnwsene\n\
                    \sewnenenenesenwsewnenwwwse\n\
                    \wenwwweseeeweswwwnwwe\n\
                    \wsweesenenewnwwnwsenewsenwwsesesenwne\n\
                    \neeswseenwwswnwswswnw\n\
                    \nenwswwsewswnenenewsenwsenwnesesenew\n\
                    \enewnwewneswsewnwswenweswnenwsenwsw\n\
                    \sweneswneswneneenwnewenewwneswswnese\n\
                    \swwesenesewenwneswnwwneseswwne\n\
                    \enesenwswwswneneswsenwnewswseenwsese\n\
                    \wnwnesenesenenwwnenwsewesewsesesew\n\
                    \nenewswnwewswnenesenwnesewesw\n\
                    \eneswnwswnwsenenwnwnwwseeswneewsenese\n\
                    \neswnwewnwnwseenwseesewsenwsweewe\n\
                    \wseweeenwnesenwwwswnew" & lines
      it "runs part a for the example" $ do
        Day24.day24 example `shouldBe` 10
      
      let moves = Day24.parse example
          g0 = Day24.grid moves
      it "runs the example for a day or two" $ do
        
        (g0 & iterate Day24.next & map Set.size & take 11) `shouldBe` [10, 15, 12, 25, 14, 23, 28, 41, 37, 49, 37]

      it "runs for 100 days" $ do
        Set.size (Day24.after 100 g0) `shouldBe` 2208

    describe "day 25" $ do
      let example = ["5764801", "17807724"]
      it "works out the log" $ do
        Day25.pLog 5764801 1 0 `shouldBe` 8
        Day25.pLog 17807724 1 0 `shouldBe` 11
      it "works out the enc key" $ do
        17807724 ^ 8 `mod` Day25.modulus `shouldBe` 14897079
        5764801 ^ 11 `mod` Day25.modulus `shouldBe` 14897079
      it "solves part a" $ do
        Day25.day25 example `shouldBe` (14897079,14897079)