/** @flow */
import R from 'ramda';

import { into, toMaybeWhenEmpty } from './folktale';
import { filter, map, reduce } from './array';
import { split } from './string';

type ThrowWhenANumberIsNegative = (array: number[]) => number[];
const throwWhenANumberIsNegative: ThrowWhenANumberIsNegative = array => {
  const isNumberNegative = number => number < 0;
  const negativeNumberIndex = array.findIndex(isNumberNegative);

  if (negativeNumberIndex === -1) {
    return array;
  } else {
    throw new Error('negatives not allowed');
  }
};

type GetSeparators = (delimiterIndex: number) => (string: string) => string;
const getSeparators: GetSeparators = delimiterIndex => string => {
  if (delimiterIndex === -1) {
    return '\n';
  } else {
    const [delimeter] = string.split('\n');
    if (delimeter.charAt(2) === '[') {
      return '***';
    } else {
      return delimeter.charAt(2);
    }
  }
};

type CreateSanitizeSeparators = (
  delimiterIndex: number
) => (string: string) => (string: string) => string;
const createSanitizeSeparators: CreateSanitizeSeparators = delimiterIndex => separator => string => {
  if (delimiterIndex === -1) {
    return string.replace(separator, ',');
  } else {
    const [_, numbers] = string.split('\n');
    return numbers.replace(separator, ',');
  }
};

type AddNumbers = (string: string) => number;
const addNumbers: AddNumbers = string => {
  const delimiterIndex = string.indexOf('//');

  const sanitizeSeparators = R.pipe(
    getSeparators(delimiterIndex),
    createSanitizeSeparators(delimiterIndex)
  )(string);

  const toNumbers = map(string => parseInt(string));
  const filterBigNumbers = filter(number => number < 1000);
  const reduceNumbers = reduce((acc, number) => acc + number)(0);

  const sum = R.pipe(
    toMaybeWhenEmpty,
    into(sanitizeSeparators),
    into(split),
    into(toNumbers),
    into(throwWhenANumberIsNegative),
    into(filterBigNumbers),
    into(reduceNumbers)
  )(string);

  return sum.getOrElse(0);
};

export { addNumbers as default };
