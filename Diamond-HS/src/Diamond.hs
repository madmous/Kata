module Diamond (makeDiamondWith) 
  where

import Data.List (intercalate, length, replicate, reverse, init)
import Data.Char (ord)

makeDiamondWith :: Char -> String
makeDiamondWith 'A' = "A"
makeDiamondWith letter =
  let letters = ['A'..letter]
      verticalLines = letters ++ reverse (init letters)
  in unlines $ map (toRow $ length verticalLines) verticalLines

toRow :: Int -> Char -> String
toRow width 'A' = pad width "A"
toRow width letter = 
  let withInnerSpaces = [letter] ++ createSpaces (countInnerSpaces letter) ++ [letter]
  in pad width withInnerSpaces

pad :: Int -> String -> String
pad width string = 
  let outerSpaces = createSpaces $ div (width - length string) 2
  in outerSpaces ++ string ++ outerSpaces

createSpaces :: Int -> String
createSpaces count = replicate count ' '

countInnerSpaces :: Char -> Int
countInnerSpaces char = 
  let innerSpacesByIndex = zip [0..25] (0:[1,3..])
      isLetterCharCodeIndex (alphabetIndex,_) = alphabetIndex == ord char - 65
  in snd $ head $ filter isLetterCharCodeIndex innerSpacesByIndex
