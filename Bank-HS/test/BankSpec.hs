module BankSpec where

import Bank(deposit, withdraw, Account(..))
import Test.Hspec
  
{-# ANN spec "HLint: Redundunt do" #-}

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Bank" $ do
    it "" $ do
      withdraw Account { balance = 50 } 40 `shouldBe` Account { balance = 10 }
    it "" $ do
      deposit Account { balance = 50 } 40 `shouldBe` Account { balance = 90 }