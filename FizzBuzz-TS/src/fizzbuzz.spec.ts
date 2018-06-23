/** @flow */

import { bless, check, forall, generator, random, shrink } from 'jsverify';
import { every, range, size, split, zip } from 'lodash/fp';

import generateOutput, { BUZZ, divisibleBy3And5, FIZZ, FIZZ_BUZZ } from './fizzbuzz';

describe('FizzBuzzProperty', () => {
  const checkOptions = {
    quiet: true,
    tests: 26
  };

  it('Non empty', () => {
    const prop = check(
      forall(arbListLength(), listLength => {
        const output = generateOutput(listLength);

        return size(output) !== 0;
      }),
      checkOptions
    );

    expect(prop).toBe(true);
  });

  it(`Numbers only divisible by 3 are represented by ${FIZZ}`, () => {
    const prop = check(
      forall(arbListLength(), listLength => {
        const output = generateOutput(listLength);

        const onlyDivisibleBy3 = keepNumbersDivisibleBy(3)(5)(output);

        return every(([_, v]) => v === FIZZ)(onlyDivisibleBy3);
      }),
      checkOptions
    );

    expect(prop).toBe(true);
  });

  it(`Numbers only divisible by 5 are represented by ${BUZZ}`, () => {
    const prop = check(
      forall(arbListLength(), listLength => {
        const output = generateOutput(listLength);

        const onlyDivisibleBy5 = keepNumbersDivisibleBy(5)(3)(output);

        return every(([_, v]) => v === BUZZ)(onlyDivisibleBy5);
      }),
      checkOptions
    );

    expect(prop).toBe(true);
  });

  it(`Numbers divisible by 3 and 5 are represented by ${FIZZ_BUZZ}`, () => {
    const prop = check(
      forall(arbListLength(), listLength => {
        const output = generateOutput(listLength);

        const onlyDivisibleBy3And5 = keepNumbersDivisibleBy3And5(output);

        return every(valueIsFizzAndOrBuzz)(onlyDivisibleBy3And5);
      }),
      checkOptions
    );

    expect(prop).toBe(true);
  });
});

const arbListLength = () =>
  bless({
    generator: generator.bless(() => random(1, 50)),
    show: val => val,
    shrink: shrink.noop
  });

const valueIsFizzAndOrBuzz = ([value, output]) => {
  if (divisibleBy3And5(value)) {
    return output === FIZZ_BUZZ;
  } else {
    throw new Error(`Expected value to be 3 and/or 5 and got ${output}`);
  }
};

const keepNumbersDivisibleBy = (nbr: number) => (nbr2: number) => (nbrs: string) => {
  const outputWithIndexes = addIndex(split('\n')(nbrs));

  return outputWithIndexes.filter(([index, _]) => index % nbr === 0 && index % nbr2 !== 0);
};

const keepNumbersDivisibleBy3And5 = (nbrs: string) => {
  const outputWithIndexes = addIndex(split('\n')(nbrs));

  return outputWithIndexes.filter(([index, _]) => divisibleBy3And5(index));
};

type AddIndex = (str: string[]) => Array<[number, string]>;
const addIndex: AddIndex = (collection: string[]) =>
  zip(range(1, size(collection) + 1))(collection);
