module Bowling (score) where

import Data.Char (digitToInt)
import Data.List (head, map, sum, tail)
import Text.Printf (printf)
  
data Frame = Strike | Spare Int | Simple Int Int
  deriving (Eq, Show)
  
score :: String -> Int
score [] = 0
score input = calculate . zip [1..] . parse $ input

parse :: String -> [Frame]
parse [] = []
parse ('X':frames) = Strike : parse frames
parse (frame:'/':frames) = (Spare $ digitToInt frame) : parse frames
parse ['-'] = [Simple 0 0]
parse [frame] = [Simple (digitToInt frame) 0]
parse ('-':'-':frames) = Simple 0 0 : parse frames
parse (frame:'-':frames) = Simple (digitToInt frame) 0 : parse frames
parse ('-':frame:frames) = Simple 0 (digitToInt frame) : parse frames
parse (frame1:frame2:frames) = Simple (digitToInt frame1) (digitToInt frame2) : parse frames

calculate :: [(Int, Frame)] -> Int
calculate [] = 0
calculate [x] =
  case x of 
    (_, Simple roll1 roll2) -> roll1 + roll2
    _ -> 10
calculate [frame1, frame2] =
  case frame1 of 
    (_, Simple roll1 roll2) -> roll1 + roll2 + calculate [frame2]
    (_, Spare _) -> 10 + getOneRollFrom frame2 + calculate [frame2]
    (10, Strike) -> 10 + calculate [frame2]
    (11, Strike) -> 10 + calculate [frame2]
    (_, Strike) -> 10 + getTwoRollsFrom [frame2] + calculate [frame2]
calculate (frame1:frame2:frame3:frames) =
  case frame1 of 
    (_, Simple roll1 roll2) -> roll1 + roll2 + calculate(frame2:frame3:frames)
    (10, _) -> 10 + calculate(frame2:frame3:frames)
    (11, _) -> 10 + calculate(frame2:frame3:frames)
    (_, Spare _) -> 10 + getOneRollFrom frame2 + calculate(frame2:frame3:frames)
    (_, Strike) -> 10 + getTwoRollsFrom [frame2,frame3] + calculate(frame2:frame3:frames)

getOneRollFrom :: (Int, Frame) -> Int
getOneRollFrom frame = 
  case frame of
    (_, Simple roll1 _) -> roll1
    (_, Spare roll1) -> roll1
    (_, Strike) -> 10

getTwoRollsFrom :: [(Int, Frame)] -> Int
getTwoRollsFrom [frame] = 
  case frame of
    (_, Simple roll1 roll2) -> roll1 + roll2
    _ -> 10
getTwoRollsFrom [frame, frame2] = 
  case frame of
    (_, Simple roll1 roll2) -> roll1 + roll2
    (_, Spare _) -> 10
    (_, Strike) -> 10 + getOneRollFrom frame2