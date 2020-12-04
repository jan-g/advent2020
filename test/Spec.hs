import Test.Hspec
import Control.Exception (evaluate)

import Data.Array
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad
import Data.List.Split (splitOn)

import qualified Day1
import qualified Day3
import qualified Day4


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