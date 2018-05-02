/** @flow */

import {
  doTransactionBetweenAccounts,
  doTransaction,
} from './main';

import type { TransactionType, Transaction, Account } from './main';

type ATransaction = $Shape<Transaction> => (type: TransactionType) => Transaction;
type AnAccount = $Shape<Account> => Account;

const anAccount: AnAccount = accountProps => {
  const defaultAccount = {
    amount: 100,
    transactions: [],
  };

  const account = {
    ...defaultAccount,
    ...accountProps,
  };

  return account;
};

const aTransaction: ATransaction = transactionProps => type => {
  const defaultTransaction = {
    amount: 100,
    date: '2018-02-01',
  };

  const account = {
    ...defaultTransaction,
    ...transactionProps,
    type,
  };

  return account;
};

describe('Acount deposit', () => {
  it('should deposit money to an account when issued', async () => {
    // given
    const account = anAccount();
    const transaction = aTransaction()(
      'DEPOSIT'
    );

    // when
    const newAccount = doTransaction(account)(transaction);

    // then
    expect(newAccount).toEqual({ amount: 200, transactions: [transaction] });
  });
});

describe('Acount withdrawal', () => {
  it('should withdraw money when the account amount is not empty', async () => {
    // given
    const account = anAccount();
    const transaction = aTransaction()(
      'WITHDRAWAL'
    );

    // when
    const newAccount = doTransaction(account)(transaction);

    // then
    expect(newAccount).toEqual({ amount: 0, transactions: [transaction] });
  });
});

describe('Transfer between accounts', () => {
  it('should withdraw money from one account and deposit it to another', async () => {
    // given
    const fromAccount = anAccount();
    const toAccount = anAccount(150);

    const transaction = aTransaction()('WITHDRAWAL');

    // when
    const accounts = doTransactionBetweenAccounts(fromAccount)(transaction)(
      toAccount
    );

    // then
    expect(accounts).toEqual({
      fromAccount: {
        amount: 0,
        transactions: [transaction],
      },
      toAccount: {
        amount: 250,
        transactions: [{ ...transaction, type: 'DEPOSIT' }],
      },
    });
  });
});