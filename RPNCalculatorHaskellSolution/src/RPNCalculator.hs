module RPNCalculator (generateOutput)
  where

import Data.List (foldl, head)

generateOutput :: String -> Int
generateOutput "" = 0
generateOutput input = head . calculate . words $ input

calculate :: [String] -> [Int]
calculate = foldl foldingFunction []
  where foldingFunction (x:y:xs) "+" = (x + y) : xs
        foldingFunction (x:y:xs) "-" = (y - x) : xs
        foldingFunction (x:y:xs) "*" = (x * y) : xs
        foldingFunction xs number = read number : xs