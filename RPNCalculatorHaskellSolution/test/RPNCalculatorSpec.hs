module RPNCalculatorSpec where
  
import RPNCalculator (generateOutput)
import Test.Hspec

{-# ANN spec "HLint: Redundunt do" #-}

main :: IO ()
main = hspec spec
  
spec :: Spec
spec =
  describe "RPN" $ do
    it "Empty" $ do
      generateOutput "" `shouldBe` 0
    
    it "Sum" $ do
      generateOutput "5 3 +" `shouldBe` 8

    it "Subtract" $ do
      generateOutput "5 3 -" `shouldBe` 2

    it "Multiply" $ do
      generateOutput "5 3 *" `shouldBe` 15


