/** @flow */
import { pipe } from 'ramda';

import { getValueOrFail, into, Result } from './folktale';
import { findIndex, filter, join, map, reduce } from './array';
import { charAt, slice, split } from './string';

type CheckForNegativeNumbers = (array: number[]) => Result<string, number[]>;
const checkForNegativeNumbers: CheckForNegativeNumbers = array => {
  const isNumberNegative = number => number < 0;
  const getNegativeNumberIndex = findIndex(isNumberNegative);
  const createResult = negativeNumberIndex => {
    if (negativeNumberIndex === -1) {
      return Result.Ok(array);
    } else {
      return Result.Error('negatives not allowed');
    }
  };

  return pipe(getNegativeNumberIndex, createResult)(array);
};

type ExtractNumberFrom = (string: string) => string;
const extractNumberFrom: ExtractNumberFrom = string => {
  const [head, last] = split('\n')(string);
  const [_, delimiter] = split('//')(head);

  const getNewDelimiterIfNeeded = delimiter => {
    const firstChar = charAt(0)(delimiter);

    if (firstChar === '[') {
      return split(slice(1)(-1)(delimiter));
    } else {
      return split(delimiter);
    }
  };
  
  if (delimiter) {
    return pipe(
      getNewDelimiterIfNeeded(delimiter),
      join(','),
    )(last);
  } else {
    return string;
  }
};

type ReplaceNewLines = (delimiter: string) => (string: string) => string;
const replaceWith: ReplaceNewLines = delimiter => string => {
  return pipe(
    split(''),
    map(s => {
      if (s === delimiter) {
        return ',';
      } else {
        return s;
      }
    }),
    join('')
  )(string);
};

type FillWithZeroWhenEmpty = (input: string) => string;
const fillWithZeroWhenEmpty: FillWithZeroWhenEmpty = input => {
  if (input.length === 0) {
    return '0';
  } else {
    return input;
  }
};

type AddNumbers = (input: string) => number;
const addNumbers: AddNumbers = input => {
  const BIG_NUMBERS = 1000;
  
  return pipe(
    fillWithZeroWhenEmpty,
    extractNumberFrom,
    replaceWith('\n'),
    split(','),
    map(val => parseInt(val)),
    checkForNegativeNumbers,
    into(filter(number => number < BIG_NUMBERS)),
    into(reduce((acc, number) => acc + number)(0)),
    getValueOrFail
  )(input);
};

export { addNumbers as default };
