/** @flow */

/**
 * TYPES
 */

export type Amount = number;

export type TransactionType = 'DEPOSIT' | 'WITHDRAWAL';

export type Transaction = {
  amount: Amount,
  date: string,
  type: TransactionType,
};

export type Account = {
  amount: Amount,
  transactions: Transaction[],
};

/**
 * WORKFLOW
 */

type DoTransaction = (
  account: Account
) => (transaction: Transaction) => Account;

type DoTransactionBetweenAccounts = (
  fromAccount: Account
) => (
  transaction: Transaction
) => (toAccount: Account) => { fromAccount: Account, toAccount: Account };

/**
 *  MAIN PROGRAM
 */

const doTransaction: DoTransaction = account => transaction => {
  const amount =
    transaction.type === 'DEPOSIT'
      ? account.amount + transaction.amount
      : account.amount - transaction.amount;

  return {
    ...account,
    amount,
    transactions: [...account.transactions, transaction],
  };
};

const doTransactionBetweenAccounts: DoTransactionBetweenAccounts = fromAccount => transaction => toAccount => {
  const newAccountSource = doTransaction(fromAccount)(transaction);
  const depositType = transaction.type === 'DEPOSIT' ? 'WITHDRAWAL' : 'DEPOSIT';
  const transactionOnAccountDestination = { ...transaction, type: depositType };
  const newAccountDestination = doTransaction(toAccount)(
    transactionOnAccountDestination
  );

  return {
    fromAccount: newAccountSource,
    toAccount: newAccountDestination,
  };
};

export { doTransactionBetweenAccounts, doTransaction };