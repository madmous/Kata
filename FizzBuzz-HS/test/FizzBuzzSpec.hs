module FizzBuzzSpec where
  
import FizzBuzz (generateOutput)
import Test.Hspec

{-# ANN spec "HLint: Redundunt do" #-}

main :: IO ()
main = hspec spec
  
spec :: Spec
spec =
  describe "FizzBuzzProperty" $ do
    it "Non empty" $ do
      length (generateOutput [1]) `shouldNotBe` 0

    it "FizzBuzz are numbers divisible divisible by 3 and 5" $ do
      filterNumbersOnlyDivisibleBy 15 (lines (generateOutput [1..15])) `shouldBe` ["FizzBuzz"]

    it "Fizz are numbers divisible divisible by 3" $ do
      filterNumbersOnlyDivisibleBy 3 (lines (generateOutput [1..6])) `shouldBe` ["Fizz","Fizz"]

    it "Fizz are numbers divisible divisible by 5" $ do
      filterNumbersOnlyDivisibleBy 5 (lines (generateOutput [1..10])) `shouldBe` ["Buzz","Buzz"]

filterNumbersOnlyDivisibleBy :: Int -> [String] -> [String]
filterNumbersOnlyDivisibleBy value = map(\(_,x) -> x) . filter (\(x,_) -> x `mod` value == 0) . zip [1..]


