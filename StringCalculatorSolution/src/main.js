/** @flow */
import {
  findIndex,
  filter,
  indexOf,
  join,
  map,
  pipe,
  reduce,
  split,
  without,
} from 'ramda';

import { getValueOrFail, into, Result } from './folktale';
import log from './log';

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

type FilterWith = (separator: string) => (strings: string[]) => string[];
const filterWith: FilterWith = separator => strings =>
  pipe(join(''), split(separator))(strings);

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
) => (prefixDelimiter: string) => (string: string) => string;
const sanitizeNumber: SanitizeNumber = commaSeparator => newLineDelimiter => prefixDelimiter => string => {
  const replaceDelimitersWithCommas = delimiter => string =>
    map(s => {
      if (indexOf(s)(delimiter) === -1) {
        return s;
      } else {
        return ',';
      }
    })(string);
  const removeBracketsFrom = string =>
    pipe(without(['[', ']']), join(''))(string);
  const createNumber = rest => tail => {
    const delimiter = removeBracketsFrom(tail);

    return pipe(
      replaceDelimitersWithCommas(delimiter),
      filterWith(commaSeparator),
      join(',')
    )(rest);
  };
  const createNumberIfNecessary = ({ rest, tail }) => {
    if (tail === undefined) {
      return string;
    } else {
      return createNumber(rest)(tail);
    }
  };
  const getNumberAndDelimiterWithItsPrefix = string => {
    const [head, rest] = split(newLineDelimiter)(string);
    const [_, tail] = split(prefixDelimiter)(head);

    return {
      rest,
      tail,
    };
  };

  return pipe(getNumberAndDelimiterWithItsPrefix, createNumberIfNecessary)(
    string
  );
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
  const NEW_LINE_DELIMITER = '\n';
  const PREFIX_DELIMITER = '//';

  return pipe(
    fillWithZeroWhenEmpty,
    log(1),
    sanitizeNumber(COMMA_SEPARATOR)(NEW_LINE_DELIMITER)(PREFIX_DELIMITER),
    log(2),
    replaceNewLinesWith(COMMA_SEPARATOR)(NEW_LINE_DELIMITER),
    log(3),
    filterWith(COMMA_SEPARATOR),
    toInt,
    checkForNegativeNumbers,
    into(filterBigNumbers(BIG_NUMBERS)),
    into(sum),
    getValueOrFail
  )(input);
};

export { addNumbers as default };
