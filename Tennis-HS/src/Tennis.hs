module Tennis (score) 
  where

import Data.List (foldl)

data Score = Love | Fifteen | Thirty | Forty | Game
  deriving (Eq, Ord, Show, Enum) 

score :: [(Bool, Bool)] -> String
score = generateOutput . parse

parse :: [(Bool, Bool)] -> (Score, Score)
parse = head . foldl foldFunction [(Love, Love)]
  where
    foldFunction [(playerOneScore, playerTwoScore)] (hasPlayerOneScored, hasPlayerTwoScored) 
      | playerOneScore == Forty && playerTwoScore == Game && hasPlayerOneScored = [(playerOneScore, pred playerTwoScore)] 
      | playerOneScore == Game && playerTwoScore == Forty && hasPlayerTwoScored = [(pred playerOneScore, playerTwoScore)] 
      | hasPlayerOneScored = [(succ playerOneScore, playerTwoScore)] 
      | hasPlayerTwoScored = [(playerOneScore, succ playerTwoScore)] 

generateOutput :: (Score, Score) -> String
generateOutput (playerOneScore, playerTwoScore)
  | playerOneScore == Forty && playerTwoScore == Forty = "Deuce"
  | playerOneScore == Game && playerTwoScore == Forty = "Advantage Player 1"
  | playerOneScore == Forty && playerTwoScore == Game = "Advantage Player 2"
  | playerOneScore == Game = "Game Player 1"
  | playerTwoScore == Game = "Game Player 2"
  | playerOneScore == playerTwoScore = show playerOneScore ++ " " ++ "all" 
  | otherwise = show playerOneScore ++ " " ++ show playerTwoScore