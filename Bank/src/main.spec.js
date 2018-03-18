/** @flow */

import {
  doTransactionBetweenAccounts,
  doTransaction,
  createTransaction,
} from './main';

import type { Amount, Transaction, Account } from './main';

type ATransaction = (amount: Amount, date: string) => Transaction;
type AnAccount = (amount: Amount) => Account;

const anAccount: AnAccount = amount => ({ amount, transactions: [] });

describe('Account', () => {
  it('should create an account with an amount of 100', () => {
    // given
    const amount = 100;

    // when
    const account = anAccount(amount);

    // then
    expect(account).toEqual({ amount, transactions: [] });
  });
});

describe('Acount deposit', () => {
  it('should deposit money to an account when issued', async () => {
    // given
    const account = anAccount(100);
    const transaction = createTransaction({ amount: 100, date: '2018-02-01' })(
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
    const account = anAccount(100);
    const transaction = createTransaction({ amount: 100, date: '2018-02-01' })(
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
    const account = anAccount(100);
    const account2 = anAccount(150);

    const transaction = createTransaction({
      amount: 100,
      date: '2018-02-01',
    })('WITHDRAWAL');

    // when
    const accounts = doTransactionBetweenAccounts(account)(transaction)(account2);

    // then
    expect(accounts).toEqual({
      fromAccount: {
        amount: 0, transactions: [transaction]
      },
      toAccount: {
        amount: 250, transactions: [{...transaction, type: 'DEPOSIT'}]
      },
    });
  });
});
