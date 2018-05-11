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

type CommaSeparator = ',';
type NewLineDelimiter = '\n';
type PrefixDelimiter = '//';

type ReplaceIf = <T>(predicate: (value: T) => boolean) => (defaultValue: T) => (value: T) => T;
const replaceIf: ReplaceIf = predicate => defaultValue => value => {
  if (predicate(value)) {
    return value;
  } else {
    return defaultValue;
  }
};

type IsBelow = (threshold: number) => (value: number) => boolean;
const isBelow: IsBelow = threshold => value => value < threshold;

type ThrowIfNegativeNumbers = (numbers: number[]) => number[];
const throwIfNegativeNumbers: ThrowIfNegativeNumbers = numbers => {
  const negativeNumberIndex = findIndex(isBelow(0))(numbers);

  if (negativeNumberIndex === -1) {
    return numbers;
  } else {
    throw new Error('negatives not allowed');
  }
};

type ToNumber = (numberInString: string) => number;
const toNumber: ToNumber = numberInString => parseInt(numberInString, 10);

type RemoveOccurencesOf = (
  commaSeparator: CommaSeparator
) => (numbersInString: string[]) => string[];
const removeOccurencesOf: RemoveOccurencesOf = commaSeparator => numbersInString =>
  flow(join(''), split(commaSeparator))(numbersInString);

type IsNot = (delimiter: string) => (char: string) => boolean;
const isNot: IsNot = delimiter => char => char !== delimiter;

type IsPresentIn = (delimiter: string) => (numberInString: string) => boolean;
const isPresentIn: IsPresentIn = (delimiter: string) => (numberInString: string) =>
  indexOf(numberInString)(delimiter) === -1;

type IsBracket = (strings: string) => boolean;
const isBracket: IsBracket = strings => strings !== '[' || strings !== '[';

type ReplaceDelimitersIfNecessaryWith = (
  commaSeparator: CommaSeparator
) => (
  newLineDelimiter: NewLineDelimiter
) => (prefixDelimiter: PrefixDelimiter) => (input: string) => string[];
const replaceDelimitersIfNecessaryWith: ReplaceDelimitersIfNecessaryWith = commaSeparator => newLineDelimiter => prefixDelimiter => input => {
  if (startsWith(prefixDelimiter)(input)) {
    const [head, numbersInString] = split(newLineDelimiter)(input);
    const [_, maybeDelimiterWithBrackets] = split(prefixDelimiter)(head);
    const delimiter = join('')(filter(isBracket)(maybeDelimiterWithBrackets));

    return map(replaceIf(isPresentIn(delimiter))(commaSeparator))(numbersInString);
  } else {
    return map(replaceIf(isNot(newLineDelimiter))(commaSeparator))(input);
  }
};

type AddNumbers = (input: string) => number;
const addNumbers: AddNumbers = input => {
  const BIG_NUMBERS = 1000;
  const COMMA_SEPARATOR = ',';
  const NEW_LINE_DELIMITER = '\n';
  const PREFIX_DELIMITER = '//';

  return flow(
    replaceDelimitersIfNecessaryWith(COMMA_SEPARATOR)(NEW_LINE_DELIMITER)(PREFIX_DELIMITER),
    removeOccurencesOf(COMMA_SEPARATOR),
    map(toNumber),
    filter(isBelow(BIG_NUMBERS)),
    throwIfNegativeNumbers,
    sum
  )(input);
};

export { addNumbers as default };
