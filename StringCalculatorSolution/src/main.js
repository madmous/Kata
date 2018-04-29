/** @flow */
import {
  findIndex,
  filter,
  join,
  map,
  pipe,
  reduce,
  split,
  without,
} from 'ramda';

import { getValueOrFail, into, Result } from './folktale';

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
  const getNegativeNumberIndex = array => findIndex(isNumberNegative)(array);
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
const toInt: ToInt = strings => map(val => parseInt(val))(strings);

type FilterCommas = (
  commaSeparator: string
) => (emptySeparator: string) => (strings: string[]) => string[];
const filterCommas: FilterCommas = commaSeparator => emptySeparator => strings =>
  pipe(join(emptySeparator), split(commaSeparator))(strings);

type ReplaceNewLines = (
  commaSeparator: string
) => (newLineDelimiter: string) => (string: string) => string;
const replaceNewLinesWith: ReplaceNewLines = commaSeparator => newLineDelimiter => string =>
  map(string => {
    if (string === newLineDelimiter) {
      return commaSeparator;
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
  const removeBracketsFrom = string =>
    pipe(without(['[', ']']), join(''))(string);
  const removeDelimiter = separator => string => split(separator)(string);
  const getNewString = rest => tail => {
    const delimiter = removeBracketsFrom(tail);
    return pipe(removeDelimiter(delimiter), join(commaSeparator))(rest);
  };

  const [head, rest] = split(newLineDelimiter)(string);
  const [_, tail] = split(prefixDelimitor)(head);

  if (tail === undefined) {
    return string;
  } else {
    return getNewString(rest)(tail);
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
    replaceNewLinesWith(COMMA_SEPARATOR)(NEW_LINE_DELIMITER),
    filterCommas(COMMA_SEPARATOR)(EMPTY_SEPRATOR),
    toInt,
    checkForNegativeNumbers,
    into(filterBigNumbers(BIG_NUMBERS)),
    into(sum),
    getValueOrFail
  )(input);
};

export { addNumbers as default };
