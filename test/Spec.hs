import Test.Hspec
import Control.Exception (evaluate)

import Data.Array
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)

import Lib
import qualified Day1
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
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
    describe "Day1" $ do
      it "correctly runs on the test data" $ do
        (lines Day1.testData & Day1.parse) `shouldBe` [1721, 979, 366, 299, 675, 1456]
        Day1.day1 (lines Day1.testData) `shouldBe` Just (1721 * 299)

    describe "Day1b" $ do
      it "correctly runs on the test data" $ do
        let ns = lines Day1.testData & Day1.parse & Set.fromList
        Day1.findTriples 2020 ns `shouldBe` Set.fromList [979, 366, 675]

    describe "Day3" $ do
      it "correctly runs the test data" $ do
        let ls = "..##.......\n\
                 \#...#...#..\n\
                 \.#....#..#.\n\
                 \..#.#...#.#\n\
                 \.#...##..#.\n\
                 \..#.##.....\n\
                 \.#.#.#....#\n\
                 \.#........#\n\
                 \#.##...#...\n\
                 \#...##....#\n\
                 \.#..#...#.#"
        (Day3.day3 (lines ls)) `shouldBe` 7
        (Day3.day3b (lines ls)) `shouldBe` 336

    describe "Day4" $ do
      let testInput = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
                      \byr:1937 iyr:2017 cid:147 hgt:183cm\n\
                      \\n\
                      \iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
                      \hcl:#cfa07d byr:1929\n\
                      \\n\
                      \hcl:#ae17e1 iyr:2013\n\
                      \eyr:2024\n\
                      \ecl:brn pid:760753108 byr:1931\n\
                      \hgt:179cm\n\
                      \\n\
                      \hcl:#cfa07d eyr:2025 pid:166559648\n\
                      \iyr:2011 ecl:brn hgt:59in" & lines
      it "parses the password file" $ do
        let parse = Day4.parse testInput

        (length parse) `shouldBe` 4

        (Day4.hgt (parse !! 1)) `shouldBe` Nothing

        let validity = map Day4.isValid parse

        validity `shouldBe` [True, False, True, False]

      let invalid = "eyr:1972 cid:100\n\
                    \hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926\n\
                    \\n\
                    \iyr:2019\n\
                    \hcl:#602927 eyr:1967 hgt:170cm\n\
                    \ecl:grn pid:012533040 byr:1946\n\
                    \\n\
                    \hcl:dab227 iyr:2012\n\
                    \ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277\n\
                    \\n\
                    \hgt:59cm ecl:zzz\n\
                    \eyr:2038 hcl:74454a iyr:2023\n\
                    \pid:3556412378 byr:2007\n\
                    \" & lines

      let valid = "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\n\
                   \hcl:#623a2f\n\
                   \\n\
                   \eyr:2029 ecl:blu cid:129 byr:1989\n\
                   \iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm\n\
                   \\n\
                   \hcl:#888785\n\
                   \hgt:164cm byr:2001 iyr:2015 cid:88\n\
                   \pid:545766238 ecl:hzl\n\
                   \eyr:2022\n\
                   \\n\
                   \iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719\n\
                   \" & lines

      it "correctly invalidates passwords" $ do
        let parse = Day4.parse invalid

        (length parse) `shouldBe` 4

        let validity = map Day4.isMoreValid parse

        validity `shouldBe` [False, False, False, False]

      it "correctly validates passwords" $ do
        let parse = Day4.parse valid

        (length parse) `shouldBe` 4

        let validity = map Day4.isMoreValid parse

        validity `shouldBe` [True, True, True, True]

    describe "Day5" $ do
      it "correctly decodes passes" $ do
        (Day5.pass 0 "BFFFBBFRRR") `shouldBe` 567
        (Day5.pass 0 "FFFBBBFRRR") `shouldBe` 119
        (Day5.pass 0 "BBFFBBFRLL") `shouldBe` 820

    describe "Day6" $ do
      let example = "abc\n\
                    \\n\
                    \a\n\
                    \b\n\
                    \c\n\
                    \\n\
                    \ab\n\
                    \ac\n\
                    \\n\
                    \a\n\
                    \a\n\
                    \a\n\
                    \a\n\
                    \\n\
                    \b" & lines

      it "does part a for the example" $ do
                (Day6.day6 example) `shouldBe` 11
      it "does part b for the example" $ do
                (Day6.day6b example) `shouldBe` 6

    describe "day 7" $ do
      let example = "light red bags contain 1 bright white bag, 2 muted yellow bags.\n\
                    \dark orange bags contain 3 bright white bags, 4 muted yellow bags.\n\
                    \bright white bags contain 1 shiny gold bag.\n\
                    \muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\n\
                    \shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\n\
                    \dark olive bags contain 3 faded blue bags, 4 dotted black bags.\n\
                    \vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\n\
                    \faded blue bags contain no other bags.\n\
                    \dotted black bags contain no other bags." & lines
      it "parses one item" $ do
        (quickParse Day7.contentParser' "1 old grey bag") `shouldBe` Just (1, Day7.Bag "old grey")
      it "parses a list of content" $ do
        (quickParse Day7.contentParser "1 old grey bag.") `shouldBe` Just (Set.singleton (1, Day7.Bag "old grey"))
        (quickParse Day7.contentParser "3 old grey bags.") `shouldBe` Just (Set.singleton (3, Day7.Bag "old grey"))
        (quickParse Day7.contentParser "3 old grey bags, 1 bright red bag.") `shouldBe` Just (Set.fromList [(3, Day7.Bag "old grey"), (1, Day7.Bag "bright red")])

      it "parses the whole thing" $ do
        let p = Day7.parse example
        p `shouldBe` Map.fromList [(Day7.Bag "light red", Set.fromList [(1, Day7.Bag "bright white"), (2, Day7.Bag "muted yellow")])
                                  ,(Day7.Bag "dark orange", Set.fromList [(3, Day7.Bag "bright white"), (4, Day7.Bag "muted yellow")])
                                  ,(Day7.Bag "bright white", Set.fromList [(1, Day7.Bag "shiny gold")])
                                  ,(Day7.Bag "muted yellow", Set.fromList [(2, Day7.Bag "shiny gold"), (9, Day7.Bag "faded blue")])
                                  ,(Day7.Bag "shiny gold", Set.fromList [(1, Day7.Bag "dark olive"), (2, Day7.Bag "vibrant plum")])
                                  ,(Day7.Bag "dark olive", Set.fromList [(3, Day7.Bag "faded blue"), (4, Day7.Bag "dotted black")])
                                  ,(Day7.Bag "vibrant plum", Set.fromList [(5, Day7.Bag "faded blue"), (6, Day7.Bag "dotted black")])
                                  ,(Day7.Bag "faded blue", Set.fromList [])
                                  ,(Day7.Bag "dotted black", Set.fromList [])
                                  ]

      it "inverts a simple map" $ do
        let p = Day7.parse ["a b bags contain 1 c d bag."]
        (Day7.invert p) `shouldBe` Map.fromList [(Day7.Bag "c d", Set.fromList [Day7.Bag "a b"])]

      it "inverts a complex map" $ do
        let p = Day7.parse ["a b bags contain 1 c d bag, 3 e f bags."
                           ,"g h bags contain 3 c d bags, 1 i j bag."]
        (Day7.invert p) `shouldBe` Map.fromList [(Day7.Bag "c d", Set.fromList [Day7.Bag "a b", Day7.Bag "g h"])
                                                ,(Day7.Bag "e f", Set.fromList [Day7.Bag "a b"])
                                                ,(Day7.Bag "i j", Set.fromList [Day7.Bag "g h"])]

      it "expands containment" $ do
        let p = Day7.parse ["a b bags contain 1 c d bag, 3 e f bags."
                           ,"g h bags contain 3 c d bags, 1 i j bag."]
        (Day7.canContain p (Day7.Bag "e f")) `shouldBe` Set.fromList [Day7.Bag "a b"]
        (Day7.canContain p (Day7.Bag "a b")) `shouldBe` Set.fromList []
        (Day7.canContain p (Day7.Bag "i j")) `shouldBe` Set.fromList [Day7.Bag "g h"]
        (Day7.canContain p (Day7.Bag "c d")) `shouldBe` Set.fromList [Day7.Bag "a b", Day7.Bag "g h"]

      it "passes part a for the example" $ do
        let p = Day7.parse example
        (Day7.canContain p Day7.myBag) `shouldBe` Set.fromList [Day7.Bag "bright white",Day7.Bag "muted yellow",Day7.Bag "dark orange",Day7.Bag "light red"]
        (Day7.day7 example) `shouldBe` 4

      it "passes part b for the first example" $ do
        let p = Day7.parse example
        (Day7.totalForward p Day7.myBag) `shouldBe` 33
        (Day7.day7b example) `shouldBe` 32

      it "passes part b for the other example" $ do
        let ex2 = "shiny gold bags contain 2 dark red bags.\n\
                  \dark red bags contain 2 dark orange bags.\n\
                  \dark orange bags contain 2 dark yellow bags.\n\
                  \dark yellow bags contain 2 dark green bags.\n\
                  \dark green bags contain 2 dark blue bags.\n\
                  \dark blue bags contain 2 dark violet bags.\n\
                  \dark violet bags contain no other bags." & lines
            p = Day7.parse ex2
        p `shouldBe` Map.fromList [(Day7.Bag "shiny gold", Set.fromList [(2, Day7.Bag "dark red")])
                                  ,(Day7.Bag "dark red", Set.fromList [(2, Day7.Bag "dark orange")])
                                  ,(Day7.Bag "dark orange", Set.fromList [(2, Day7.Bag "dark yellow")])
                                  ,(Day7.Bag "dark yellow", Set.fromList [(2, Day7.Bag "dark green")])
                                  ,(Day7.Bag "dark green", Set.fromList [(2, Day7.Bag "dark blue")])
                                  ,(Day7.Bag "dark blue", Set.fromList [(2, Day7.Bag "dark violet")])
                                  ,(Day7.Bag "dark violet", Set.fromList [])
                                  ]
        (Day7.totalForward p Day7.myBag) `shouldBe` 127
        (Day7.day7b ex2) `shouldBe` 126

    describe "day 8" $ do
      let example = "nop +0\n\
                    \acc +1\n\
                    \jmp +4\n\
                    \acc +3\n\
                    \jmp -3\n\
                    \acc -99\n\
                    \acc +1\n\
                    \jmp -4\n\
                    \acc +6" & lines
      it "evaluates the example" $ do
        (Day8.day8 example) `shouldBe` (Day8.Repeated 5)

      it "determines all alternatives" $ do
        let instr = Day8.parse example
            variants = Day8.decorrupt instr
        (length variants) `shouldBe` 4
      it "evaluates alternatives loking for terminations" $ do
        (Day8.day8b example) `shouldBe` [Day8.Terminated 8]


    describe "day 9" $ do
      it "computes isSumOf correctly" $ do
        (Day9.isSumOf [1, 2, 3, 4, 5] 3) `shouldBe` True
        (Day9.isSumOf [2] 4) `shouldBe` False
        -- apparently not: (Day9.isSumOf [2, 2] 4) `shouldBe` True

      let example = [35
                    ,20
                    ,15
                    ,25
                    ,47
                    ,40
                    ,62
                    ,55
                    ,65
                    ,95
                    ,102
                    ,117
                    ,150
                    ,182
                    ,127
                    ,219
                    ,299
                    ,277
                    ,309
                    ,576]
      it "locates the first non-sum" $ do
        (Day9.dropSums 5 example) `shouldBe` 127

      it "works out the sequence" $ do
        (Day9.locateSequence 127 example) `shouldBe` [15,25,47,40]
      it "finds the greatest and least values of the sequence" $ do
        (Day9.answer 127 example) `shouldBe` 15 + 47

      it "works using the linear search" $ do
        (Day9.locateSequence2 127 example) `shouldBe` Just [15, 25, 47, 40]

      it "satisfies some problematical sequence searches" $ do
        (Day9.locateSequence2 5 [6, 7, 6, 5, 1, 2, 2]) `shouldBe` Just [1, 2, 2]

      it "fails gracefully" $ do
        (Day9.locateSequence2 5 [1, 1, 1]) `shouldBe` Nothing

    describe "Day 10" $ do
      let example1 = [16,10,15,5,1,11,7,19,6,12,4]
      let example2 = [28,33,18,42,31,14,46,20,48,47,24,23,49,45,19,38,39,11,1,32,25,35,8,17,7,9,4,2,34,10,3]
      it "simple example" $ do
        (Day10.partA example1) `shouldBe` 7 * 5

      it "harder example" $ do
        (Day10.partA example2) `shouldBe` 22 * 10

      it "part a for easy" $ do
        (Day10.partB example1) `shouldBe` 8

      it "part b for harder" $ do
        (Day10.partB example2) `shouldBe` 19208

      it "works out the count of sequences of 1s" $ do
        (Day10.cnt []) `shouldBe` 1
        (Day10.cnt [1]) `shouldBe` 1
        (Day10.cnt [1,1]) `shouldBe` 2
        (Day10.cnt [1,1,1]) `shouldBe` 4
        (Day10.cnt [1,1,1,1]) `shouldBe` 7   -- 1,1,1,1  1,1,2  1,2,1  1,3  2,1,1  2,2  3,1

      it "works out the count a different way" $ do
        (Day10.cnt' 0) `shouldBe` 1
        (Day10.cnt' 1) `shouldBe` 1
        (Day10.cnt' 2) `shouldBe` 2
        (Day10.cnt' 3) `shouldBe` 4
        (Day10.cnt' 4) `shouldBe` 7   -- 1,1,1,1  1,1,2  1,2,1  1,3  2,1,1  2,2  3,1

      let roughly = Day10.approx 1e-4
      it "works out the count a really different way" $ do
        Day10.cnt'' 0 `shouldSatisfy` roughly 1
        Day10.cnt'' 1 `shouldSatisfy` roughly 1
        Day10.cnt'' 2 `shouldSatisfy` roughly 2
        Day10.cnt'' 3 `shouldSatisfy` roughly 4
        Day10.cnt'' 4 `shouldSatisfy` roughly 7

    describe "day 11" $ do
      let example = "L.LL.LL.LL\n\
                    \LLLLLLL.LL\n\
                    \L.L.L..L..\n\
                    \LLLL.LL.LL\n\
                    \L.LL.LL.LL\n\
                    \L.LLLLL.LL\n\
                    \..L.L.....\n\
                    \LLLLLLLLLL\n\
                    \L.LLLLLL.L\n\
                    \L.LLLLL.LL" & lines
      let step1 = "#.##.##.##\n\
                  \#######.##\n\
                  \#.#.#..#..\n\
                  \####.##.##\n\
                  \#.##.##.##\n\
                  \#.#####.##\n\
                  \..#.#.....\n\
                  \##########\n\
                  \#.######.#\n\
                  \#.#####.##" & lines
      let step2 = "#.LL.L#.##\n\
                  \#LLLLLL.L#\n\
                  \L.L.L..L..\n\
                  \#LLL.LL.L#\n\
                  \#.LL.LL.LL\n\
                  \#.LLLL#.##\n\
                  \..L.L.....\n\
                  \#LLLLLLLL#\n\
                  \#.LLLLLL.L\n\
                  \#.#LLLL.##" & lines
      let m = Day11.parse example
      it "evolves the map" $ do
        let m1 = Day11.parse step1
        let m2 = Day11.parse step2

        Day11.step m `shouldBe` m1
        Day11.step m1 `shouldBe` m2

      let final = "#.#L.L#.##\n\
                  \#LLL#LL.L#\n\
                  \L.#.L..#..\n\
                  \#L##.##.L#\n\
                  \#.#L.LL.LL\n\
                  \#.#L#L#.##\n\
                  \..L.L.....\n\
                  \#L#L##L#L#\n\
                  \#.LLLLLL.L\n\
                  \#.#L#L#.##" & lines
      let m' = Day11.parse final
      it "runs the map to conclusion" $ do
        Day11.run m `shouldBe` m'
        Day11.occupado m' `shouldBe` 37

      let final' = "#.L#.L#.L#\n\
                   \#LLLLLL.LL\n\
                   \L.L.L..#..\n\
                   \##L#.#L.L#\n\
                   \L.L#.LL.L#\n\
                   \#.LLLL#.LL\n\
                   \..#.L.....\n\
                   \LLL###LLL#\n\
                   \#.LLLLL#.L\n\
                   \#.L#LL#.L#" & lines
      let m'' = Day11.parse final'
      it "uses the updated rules" $ do
        Day11.run' m `shouldBe` m''

    describe "Day 12" $ do
      let example = "F10\n\
                    \N3\n\
                    \F7\n\
                    \R90\n\
                    \F11" & lines
      let ms = Day12.parse example
      it "passes part a" $ do
        (Day12.go ms 0 0 1 0) `shouldBe` (17, -8, 0, -1)
        Day12.day12 example `shouldBe` 25
      it "passes part b" $ do
        (Day12.go' ms 0 0 10 1) `shouldBe` (214, -72, 4, -10)
        Day12.day12b example `shouldBe` 286

    describe "day 13" $ do
      let example = "939\n\
                    \7,13,x,x,59,x,31,19" & lines
      it "solves the simple example" $ do
        let (e, d) = Day13.parse example
            d' = catMaybes d
        (Day13.partA e d') `shouldBe` (59, 5)
        (Day13.day13 example) `shouldBe` 59 * 5

      forM_ [("5", 0)
            ,("5,5", 0)
            ,("2,3", 2)
            ,("2,3,4", 2)
            ,("2,x,4", 2)
            ,("2,3,4,5", 2)
            ,("7,13,x,x,59,x,31,19", 1068781)
            ,("17,x,13,19", 3417)
            ,("67,7,59,61", 754018)
            ,("67,x,7,59,61", 779210)
            ,("67,7,x,59,61", 1261476)
            ,("1789,37,47,1889", 1202161486)
            ] $ \(example, answer) -> do
          it ("solves example " ++ (show example)) $ do
            (Day13.partB $ Day13.parse' example) `shouldBe` answer

      forM_ [(240, 46), (199, 326)] $ \(a, b) -> do
        let (g, s, t) = euc a b
        it ("finds gcd of " ++ (show a) ++ " and " ++ (show b)) $ do
          g `shouldBe` gcd a b
        it ("performs extended euclidean algorithm for " ++ (show a) ++ " and " ++ (show b)) $ do
          s * a + t * b `shouldBe` gcd a b

      forM_ [(1, 7, -1)
            ,(3, 5, 9)
            ,(19, 6, 1)
            ] $ \(a, b, c) -> do
        it ("finds m and f such that x = mr + f are solutions to " ++ (show a) ++ "x + " ++ (show b) ++ "y = " ++ (show c)) $ do
          let ((m, f), (m', f')) = solveDiophantine a b c
          forM_ [-30 .. 30] $ \r -> do
            let x = m * r + f
                y = m' * r + f'
            a * x + b * y `shouldBe` c

    describe "Day 14" $ do
      let example = "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n\
                    \mem[8] = 11\n\
                    \mem[7] = 101\n\
                    \mem[8] = 0" & lines
      let prog = Day14.parse example
      it "applies the simple instruction" $ do
        let Day14.Memory a o m = Day14.run Day14.nullMemory (take 2 prog)
        m `shouldBe` Map.singleton 8 73
      it "runs the sample program" $ do
        let Day14.Memory a o m = Day14.run Day14.nullMemory prog
        m `shouldBe` Map.fromList [(8, 64), (7, 101)]
      it "tots up the result" $ do
        let x = Day14.run Day14.nullMemory prog
        Day14.sumValues x `shouldBe` 165
