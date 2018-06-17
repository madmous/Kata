module BowlingSpec where

import Bowling (score)
import Test.Hspec

{-# ANN spec "HLint: Redundunt do" #-}

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "BowlingProperty" $ do
    it "Frame score is 0 when game has not started" $ do
      score "" `shouldBe` 0

    it "Frame score is 0 when player only misses" $ do
      score "------" `shouldBe` 0

    it "Frame score is the sum of the pins knocked down when player does not strike or spare" $ do
      score "-9-9-9-9-9-9-9-9-9XXX" `shouldBe` (9 * 9) + 30

    it "Frame score is 10 plus the number of pins knocked down on the next throw when player spares" $ do
      score "5/5/5/5/5/5/5/5/5/5/" `shouldBe` ((10 + 5) * 9) + 10

    it "Frame score is 10 plus the number of pins knocked down on the next two throws when player strikes" $ do
      score "XX" `shouldBe` 30