import {
  camelCase,
  filter,
  findIndex,
  flow,
  indexOf,
  join,
  map,
  reduce,
  split,
  startsWith,
  sum,
  without
} from 'lodash/fp';

import log from './log';

type NewLineDelimiter = '\n';
type CommaSeparator = ',';

type ReplaceIf = <T>(predicate: (value: T) => boolean) => (defaultValue: T) => (value: T) => T;
const replaceIf: ReplaceIf = predicate => defaultValue => value => {
  if (predicate(value)) {
    return value;
  } else {
    return defaultValue;
  }
};

type CheckForNegativeNumbers = (numbers: number[]) => number[];
const checkForNegativeNumbers: CheckForNegativeNumbers = numbers => {
  const toResult = (negativeNumberIndex: number) => {
    if (negativeNumberIndex === -1) {
      return numbers;
    } else {
      throw new Error('negatives not allowed');
    }
  };
  const isNegative = (num: number) => num < 0;

  return flow(findIndex(isNegative), toResult)(numbers);
};

type ReplaceNewLineDelimiterWith = (
  commaSeparator: CommaSeparator
) => (delimiter: NewLineDelimiter) => (string: string) => string;
const replaceNewLineDelimiterWith: ReplaceNewLineDelimiterWith = commaSeparator => delimiter => string => {
  const isNotADelimiter = (char: string) => char !== delimiter;

  return flow(split(''), map(replaceIf(isNotADelimiter)(commaSeparator)), join(''))(string);
};

type FilterWith = (commaSeparator: CommaSeparator) => (strings: string[]) => string[];
const filterWith: FilterWith = commaSeparator => (strings: string[]) =>
  flow(join(''), split(commaSeparator))(strings);

type IsDelimiterIn = (string: string) => (character: string) => boolean;
const isDelimiterIn: IsDelimiterIn = string => character => indexOf(character)(string) === -1;

type ReplaceDelimitersThenCollect = (
  commaSeparator: CommaSeparator
) => (
  newLineDelimiter: NewLineDelimiter
) => (prefixDelimiter: string) => (character: string) => string[];
const replaceDelimitersThenCollect: ReplaceDelimitersThenCollect = commaSeparator => newLineDelimiter => prefixDelimiter => character => {
  const filterWith = (commaSeparator: CommaSeparator) => (strings: string[]) =>
    flow(join(''), split(commaSeparator))(strings);

  const [head, numbers] = split(newLineDelimiter)(character);
  const [_, delimiter] = split(prefixDelimiter)(head);
  const removedBrackets = join('')(
    filter((strings: string) => strings !== '[' || strings !== '[')(delimiter)
  );

  return flow(
    map(replaceIf(isDelimiterIn(removedBrackets))(commaSeparator)),
    filterWith(commaSeparator)
  )(numbers);
};

type CollectNumbersToAdd = (
  commaSeparator: CommaSeparator
) => (
  newLineDelimiter: NewLineDelimiter
) => (prefixDelimiter: string) => (character: string) => string[];
const collectNumbersToAdd: CollectNumbersToAdd = commaSeparator => newLineDelimiter => prefixDelimiter => character => {
  if (startsWith(prefixDelimiter)(character)) {
    return replaceDelimitersThenCollect(commaSeparator)(newLineDelimiter)(prefixDelimiter)(
      character
    );
  } else {
    return flow(
      replaceNewLineDelimiterWith(commaSeparator)(newLineDelimiter),
      split(commaSeparator)
    )(character);
  }
};

type IsBelow = (threshold: number) => (value: number) => boolean;
const isBelow: IsBelow = threshold => value => value < threshold;

type ToInt = (character: string) => number;
const toInt: ToInt = character => parseInt(character, 10);

type AddNumbers = (input: string) => number;
const addNumbers: AddNumbers = input => {
  const BIG_NUMBERS = 1000;
  const COMMA_SEPARATOR = ',';
  const NEW_LINE_DELIMITER = '\n';
  const PREFIX_DELIMITER = '//';

  return flow(
    collectNumbersToAdd(COMMA_SEPARATOR)(NEW_LINE_DELIMITER)(PREFIX_DELIMITER),
    map(toInt),
    filter(isBelow(BIG_NUMBERS)),
    checkForNegativeNumbers,
    sum
  )(input);
};

export { addNumbers as default };
