/** @flow */

import {
  doTransactionBetweenAccounts,
  doTransaction,
} from './main';

import type { TransactionType, Amount, Transaction, Account } from './main';

type ATransaction = ({
  amount: Amount,
  date: string,
}) => (type: TransactionType) => Transaction;
type AnAccount = (amount: Amount) => Account;

const anAccount: AnAccount = amount => ({ amount, transactions: [] });
const aTransaction: ATransaction = ({ amount, date }) => type => ({
  amount,
  date,
  type,
});

describe('Acount deposit', () => {
  it('should deposit money to an account when issued', async () => {
    // given
    const account = anAccount(100);
    const transaction = aTransaction({ amount: 100, date: '2018-02-01' })(
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
    const transaction = aTransaction({ amount: 100, date: '2018-02-01' })(
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
    const fromAccount = anAccount(100);
    const toAccount = anAccount(150);

    const transaction = aTransaction({
      amount: 100,
      date: '2018-02-01',
    })('WITHDRAWAL');

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
