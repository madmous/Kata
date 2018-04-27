/** @flow */
import { pipe, without } from 'ramda';

import { getValueOrFail, into, Result } from './folktale';
import { findIndex, filter, join, map, reduce } from './array';
import { split } from './string';

type Sum = (numbers: number[]) => number;
const sum: Sum = numbers => reduce((acc, number) => acc + number)(0)(numbers);

type FilterBigNumbers = (
  valueToFilter: number
) => (numbers: number[]) => number[];
const filterBigNumbers: FilterBigNumbers = valueToFilter => numbers =>
  filter(number => number < valueToFilter)(numbers);

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

type ToInt = (strings: string[]) => number[];
const toInt: ToInt = strings => {
  return map(val => parseInt(val))(strings);
};

type ReplaceNewLines = (
  newDelimiter: string
) => (delimiter: string) => (string: string[]) => string[];
const replaceNewLinesWith: ReplaceNewLines = newDelimiter => delimiter => string =>
  map(string => {
    if (string === delimiter) {
      return newDelimiter;
    } else {
      return string;
    }
  })(string);

type SanitizeNumber = (
  commaSeparator: string
) => (
  newLineDelimiter: string
) => (prefixDelimitor: string) => (string: string) => string;
const sanitizeNumber: SanitizeNumber = commaSeparator => newLineDelimiter => prefixDelimitor => string => {
  const removeBracketsFrom = string => join('')(without(['[', ']'], string));

  const [head, rest] = split(newLineDelimiter)(string);
  const [_, tail] = split(prefixDelimitor)(head);

  if (tail) {
    const hh = pipe(
      split(removeBracketsFrom(tail)),
      map(s => {
        if (typeof parseInt(s) === 'number') {
          return s;
        } else {
          return commaSeparator;
        }
      }),
      join(commaSeparator)
    )(rest);

    return hh;
  } else {
    return string;
  }
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
  const COMMA_SEPARATOR = ',';
  const EMPTY_SEPRATOR = '';
  const NEW_LINE_DELIMITER = '\n';
  const PREFIX_DELIMITOR = '//';

  return pipe(
    fillWithZeroWhenEmpty,
    sanitizeNumber(COMMA_SEPARATOR)(NEW_LINE_DELIMITER)(PREFIX_DELIMITOR),
    split(EMPTY_SEPRATOR),
    replaceNewLinesWith(COMMA_SEPARATOR)(NEW_LINE_DELIMITER),
    join(EMPTY_SEPRATOR),
    split(COMMA_SEPARATOR),
    toInt,
    checkForNegativeNumbers,
    into(filterBigNumbers(BIG_NUMBERS)),
    into(sum),
    getValueOrFail
  )(input);
};

export { addNumbers as default };
