module Diamond (makeDiamondWith) 
  where

import Data.List (intercalate, length, replicate, reverse, init)
import Data.Char (ord)

makeDiamondWith :: Char -> String
makeDiamondWith 'A' = "A"
makeDiamondWith letter =
  let letters = ['A'..letter]
      verticalLines = letters ++ reverse (init letters)
  in unlines $ map (createRow $ length verticalLines) verticalLines

createRow :: Int -> Char -> String
createRow width 'A' = 
  let charactersCountPerLine = 1
      outerSpacesCountToFill = 2
      outerSpaces = createSpaces $ div (width - charactersCountPerLine) outerSpacesCountToFill
  in outerSpaces ++ "A" ++ outerSpaces
createRow width letter = 
  let charactersCountPerLine = 2
      outerSpacesCountToFill = 2
      innerSpacesCount = countInnerSpaces letter
      outerSpaces = createSpaces $ div (width - innerSpacesCount - charactersCountPerLine) outerSpacesCountToFill
  in outerSpaces ++ [letter] ++ createSpaces innerSpacesCount ++ [letter] ++ outerSpaces

countInnerSpaces :: Char -> Int
countInnerSpaces char = 
  let capitalACharacterCode = 65
      index = ord char - capitalACharacterCode
      innerSpacesByIndex = zip [0..25] (0:[1,3..])
      getInnerSpaces (f,_) = f == index
  in snd $ head $ filter getInnerSpaces innerSpacesByIndex

createSpaces :: Int -> String
createSpaces count = replicate count ' '

