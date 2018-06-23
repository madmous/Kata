module TennisSpec where
  
import Tennis (score)
import Test.Hspec

import Data.Char 

{-# ANN spec "HLint: Redundunt do" #-}

main :: IO ()
main = hspec spec
  
spec :: Spec
spec =
  describe "Tennis" $ do
    it "A player that hasn't won any points has a score of love" $ do
      last (words (score [(True, False), (True, False), (True, False)])) `shouldBe` "Love"

    it "A player who won four times wins the game" $ do
      head (words (score [(True, False), (True, False), (True, False), (True, False)])) `shouldBe` "Game"

    it "Score is 15-all if each side has won one point" $ do
      score [(False, True), (True, False)] `shouldBe` "Fifteen all"
      
    it "Score is 30-all if each side has won two points" $ do
      score [(False, True), (True, False), (False, True), (True, False)] `shouldBe` "Thirty all"

    it "Score is deuce if both players have won three points" $ do
      score [(True, False), (True, False), (True, False), (False, True), (False, True), (False, True)] `shouldBe` "Deuce"

    it "Any player scoring previous to a deuce has the advantage" $ do
      head (words (score [(True, False), (True, False), (True, False), (False, True), (False, True), (False, True), (False, True)])) `shouldBe` "Advantage"
    
    it "Any player scoring previous to an advantage for the other player brings back the score to deuce" $ do
      score [(True, False), (True, False), (True, False), (False, True), (False, True), (False, True), (True, False), (False, True)] `shouldBe` "Deuce"
