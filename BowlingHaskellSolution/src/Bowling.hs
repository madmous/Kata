module Bowling (score) where

import Data.Char (digitToInt)
import Data.List (head, map, sum, tail)
import Text.Printf (printf)
  
data Frame = Strike | Spare Int | Simple Int Int
  deriving (Eq, Show)
  
score :: String -> Int
score [] = 0
score input = calculate . addIndexToFrames . parse $ input

parse :: String -> [Frame]
parse [] = []
parse ['X'] = [Strike]
parse ['-'] = [Simple 0 0]
parse [frame] = [Simple (digitToInt frame) 0]
parse ('X':frames) = Strike : parse frames
parse (frame:'/':frames) = (Spare $ digitToInt frame) : parse frames
parse ('-':'-':frames) = Simple 0 0 : parse frames
parse (frame:'-':frames) = Simple (digitToInt frame) 0 : parse frames
parse ('-':frame:frames) = Simple 0 (digitToInt frame) : parse frames
parse (frame1:frame2:frames) = Simple (digitToInt frame1) (digitToInt frame2) : parse frames

addIndexToFrames :: [Frame] -> [(Int, Frame)]
addIndexToFrames = zip [1..]

calculate :: [(Int, Frame)] -> Int
calculate [] = 0
calculate [x] =
  case x of 
    (_, Simple roll1 roll2) -> roll1 + roll2
    _ -> maximumScore
  where maximumScore = 10
calculate (frame1:frame2:frames) =
  case frame1 of 
    (_, Simple roll1 roll2) -> roll1 + roll2 + calculate(frame2:frames)
    (10, Spare _) -> maximumScore + calculate(frame2:frames)
    (10, Strike) -> maximumScore + calculate(frame2:frames)
    (11, Spare _) -> maximumScore + calculate(frame2:frames)
    (11, Strike) -> maximumScore + calculate(frame2:frames)
    (_, Spare _) -> maximumScore + bonusPointsWhen "Spare" frame2 + calculate(frame2:frames)
    (_, Strike) -> maximumScore + bonusPointsWhen "Strike" frame2 + calculate(frame2:frames)
  where maximumScore = 10

bonusPointsWhen :: String -> (Int, Frame) -> Int
bonusPointsWhen "Spare" frame = getOneRollFrom frame
bonusPointsWhen "Strike" frame = getTwoRollsFrom frame

getOneRollFrom :: (Int, Frame) -> Int
getOneRollFrom frame = 
  case frame of
    (_, Simple roll1 _) -> roll1
    (_, Spare roll1) -> roll1
    (_, Strike) -> maximumScore
  where maximumScore = 10

getTwoRollsFrom :: (Int, Frame) -> Int
getTwoRollsFrom frame = 
  case frame of
    (_, Simple roll1 roll2) -> roll1 + roll2
    _ -> maximumScore
  where maximumScore = 10