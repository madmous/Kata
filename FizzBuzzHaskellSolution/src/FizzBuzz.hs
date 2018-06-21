module FizzBuzz (generateOutput)
  where

generateOutput :: [Integer] -> String
generateOutput inputs = unlines $ map parse inputs

parse :: Integer -> String
parse v
  | divisibleBy5And3 == 0 = "FizzBuzz"
  | divisibleBy3 == 0 = "Fizz"
  | divisibleBy5 == 0 = "Buzz"
  | otherwise = show v
  where divisibleBy5And3 = v `mod` 15
        divisibleBy3 = v `mod` 3
        divisibleBy5 = v `mod` 5