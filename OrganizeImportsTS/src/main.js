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
  startsWith,
  without,
} from 'ramda';

import { getValueOrFail, into, Maybe, Result } from './folktale';
// import log from './log';

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
  const toResult = negativeNumberIndex => {
    if (negativeNumberIndex === -1) {
      return Result.Ok(array);
    } else {
      return Result.Error('negatives not allowed');
    }
  };

  return pipe(getNegativeNumberIndex, toResult)(array);
};

type ToInt = (strings: string[]) => number[];
const toInt: ToInt = strings => map(val => parseInt(val))(strings);

type FilterWith = (separator: string) => (strings: string[]) => string[];
const filterWith: FilterWith = separator => strings =>
  pipe(join(''), split(separator))(strings);

type ReplaceDelimiterWith = (
  separator: string
) => (delimiter: string) => (string: string) => string;
const replaceDelimiterWith: ReplaceDelimiterWith = separator => delimiter => string =>
  map(string => {
    if (string === delimiter) {
      return separator;
    } else {
      return string;
    }
  })(string);

type ReplaceDelimiterIfPresentWith = (
  separator: string
) => (delimiter: string) => (string: string) => string;
const replaceDelimiterIfPresentWith: ReplaceDelimiterIfPresentWith = separator => delimiter => string =>
  map(s => {
    if (indexOf(s)(delimiter) === -1) {
      return s;
    } else {
      return separator;
    }
  })(string);

type ToNumberWithoutDelimitersIfNecessary = (
  separator: string
) => (
  delimiter: string
) => (
  prefixDelimiter: string
) => ({ rest: Maybe<string>, tail: string }) => string;
const toNumberWithoutDelimitersIfNecessary: ToNumberWithoutDelimitersIfNecessary = separator => delimiter => prefixDelimiter => ({
  rest,
  tail,
}) => {
  const removeBracketsFrom = string =>
    pipe(without(['[', ']']), join(''))(string);
  const toNumberWithoutDelimiters = rest => tail => {
    const delimiter = removeBracketsFrom(tail);

    return pipe(
      replaceDelimiterIfPresentWith(separator)(delimiter),
      filterWith(separator),
      join(separator)
    )(rest);
  };

  return rest
    .map(value => toNumberWithoutDelimiters(value)(tail))
    .getOrElse(tail);
};

type ToNumberAndDelimiterWithItsPrefix = (
  delimiter: string
) => (
  prefixDelimiter: string
) => (string: string) => { rest: Maybe<string>, tail: string };
// TODO: find a name for { rest: Maybe<string>, tail: string } : character ?
const toNumberAndDelimiterWithItsPrefix: ToNumberAndDelimiterWithItsPrefix = delimiter => prefixDelimiter => string => {
  if (startsWith(prefixDelimiter)(string)) {
    const [head, rest] = split(delimiter)(string);
    const [_, tail] = split(prefixDelimiter)(head);

    return {
      rest: Maybe.Just(rest),
      tail,
    };
  } else {
    return {
      rest: Maybe.Nothing(),
      tail: string,
    };
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
  const NEW_LINE_DELIMITER = '\n';
  const PREFIX_DELIMITER = '//';

  return pipe(
    fillWithZeroWhenEmpty,
    toNumberAndDelimiterWithItsPrefix(NEW_LINE_DELIMITER)(PREFIX_DELIMITER),
    toNumberWithoutDelimitersIfNecessary(COMMA_SEPARATOR)(NEW_LINE_DELIMITER)(
      PREFIX_DELIMITER
    ),
    replaceDelimiterWith(COMMA_SEPARATOR)(NEW_LINE_DELIMITER),
    filterWith(COMMA_SEPARATOR),
    toInt,
    checkForNegativeNumbers,
    into(filterBigNumbers(BIG_NUMBERS)),
    into(sum),
    getValueOrFail
  )(input);
};

export { addNumbers as default };
