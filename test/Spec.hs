import Test.Hspec
import Control.Exception (evaluate)

import Data.Array
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad
import Data.List.Split (splitOn)

import qualified Day1


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
