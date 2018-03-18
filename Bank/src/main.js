/** @flow */

/**
 * TYPES
 */

opaque type ValidatedDate = string;

export type Amount = number;

type TransactionType = 'DEPOSIT' | 'WITHDRAWAL';

export type Transaction = {
  amount: Amount,
  date: ValidatedDate,
  type: TransactionType,
};

export type Account = {
  amount: Amount,
  transactions: Transaction[],
};

/**
 * WORKFLOW
 */

type IsRegexValid = (regex: RegExp) => (value: string) => boolean;

type DoTransaction = (
  account: Account
) => (transaction: Transaction) => Account;

type CreateTransaction = ({
  amount: Amount,
  date: string,
}) => (type: TransactionType) => Transaction;

type DoTransactionBetweenAccounts = (
  fromAccount: Account
) => (
  transaction: Transaction
) => (
  toAccount: Account
) => { fromAccount: Account, toAccount: Account };

/**
 *  MAIN PROGRAM
 */

const isRegexValid: IsRegexValid = regex => value =>
  value.match(regex) !== null;

const createTransaction: CreateTransaction = ({ amount, date }) => type => {
  const isValid = isRegexValid(/^\d{4}-\d{2}-\d{2}$/)(date);

  if (!isValid) {
    throw new Error('Enter a valid date format');
  }

  return {
    amount,
    date,
    type,
  };
};

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
  const transactionOnAccountDestination = createTransaction({ ...transaction })(
    depositType
  );
  const newAccountDestination = doTransaction(toAccount)(
    transactionOnAccountDestination
  );

  return {
    fromAccount: newAccountSource,
    toAccount: newAccountDestination,
  };
};

export { doTransactionBetweenAccounts, createTransaction, doTransaction };
