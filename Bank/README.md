## Rules

+ Deposits and withdrawals can be maded into an account
+ Each deposit or withdrawal will have the amount of the operation and the date of the operation
+ You should be able to transfer between accounts. A transfer will appear as a withdrawal in the account of the transferor and as a deposit on the account of the transferee.
+ A statement can be requested at any time. The statement will contain for each entry the date, the amount of deposition, the amount of withdrawal (only one of the two should have a value), and the balance of the account after the entry.
+ Headers should be shown on the statement.
+ You should be able to filter the statement (only deposits, only withdrawals, date)

## Changes

+ The information must be stored on a file or database.

## Coding standard

+ Name everything (meaningful, searchable, explanatory)
+ No mutable state
+ Functions should not take any boolean argument
+ Encapsulate conditinonals in functions
+ Code should be read from top to bottom with function callers and callee being close
+ Functions should be 50 lines long
+ Test functions should be 30 lines long
+ One assertion per test