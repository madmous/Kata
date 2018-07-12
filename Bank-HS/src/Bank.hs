module Bank (deposit, withdraw, Account(..)) 
  where

newtype Account = Account { balance :: Int } deriving (Eq, Show)

withdraw :: Account -> Int -> Account
withdraw Account { balance = oldBalance } amount = Account { balance = oldBalance - amount }

deposit :: Account -> Int -> Account
deposit Account { balance = oldBalance } amount = Account { balance = oldBalance + amount }
