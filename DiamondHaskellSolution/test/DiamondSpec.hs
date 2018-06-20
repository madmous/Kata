module DiamondSpec where

import Data.List (head, length, nub)
import Data.Char (isLetter, isSpace)

import Diamond (makeDiamondWith)
import Test.Hspec

{-# ANN spec "HLint: Redundunt do" #-}

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "DiamondProperty" $ do
    it "Non empty" $ do
      length (makeDiamondWith 'A') `shouldNotBe` 0
    
    it "Rows must contain the correct letters, in the correct order" $ do
      removeDuplicateLetters (makeDiamondWith 'B') `shouldBe` ["A","B","A"]

    it "Diamond is as wide as it's high" $ do
      countRowLengths (makeDiamondWith 'C') `shouldBe` [5,5,5,5,5]

    it "All rows except top and bottom have two identical letters" $ do
      countOccurencesWithoutHeadAndLast (makeDiamondWith 'C') `shouldBe` [2,2,2]
      
    it "Rows must have a symmetric contour" $ do
      countLeadingAndTrailingSpaces (makeDiamondWith 'B') `shouldBe` [(1,1),(0,0),(1,1)]

removeDuplicateLetters :: String -> [String]
removeDuplicateLetters = map nub . removeSpaces

countRowLengths :: String -> [Int]
countRowLengths = count . lines

countOccurencesWithoutHeadAndLast :: String -> [Int]
countOccurencesWithoutHeadAndLast = count . removeHeadAndLast . removeSpaces

removeSpaces :: String  -> [String]
removeSpaces = lines . filter (/= ' ')

removeHeadAndLast :: [String] -> [String]
removeHeadAndLast = tail . init

count :: [String] -> [Int]
count = map length

countLeadingAndTrailingSpaces :: String -> [(Int, Int)]
countLeadingAndTrailingSpaces = 
  let mapFunction row = (countLeadingSpaces row, countLeadingSpaces row)
  in map mapFunction . lines 

countLeadingSpaces :: String -> Int
countLeadingSpaces = length . takeWhile isSpace

countTrailingSpaces :: String -> Int
countTrailingSpaces = length . takeWhile isSpace . reverse