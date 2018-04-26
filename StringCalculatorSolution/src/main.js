/** @flow */
import { pipe } from 'ramda';

import { getValueOrFail, into, Result } from './folktale';
import {
  createArray,
  findIndex,
  fill,
  filter,
  join,
  map,
  reduce,
} from './array';
import { charAt, indexOf, replace, split } from './string';

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

type ToNumber = (character: Character) => number[];
const toNumber: ToNumber = character => {
  const SEPARATOR = ',';

  const { delimiter, characters } = character;
  const toNumbers = map(characters => parseInt(characters));
  const splitCommas = split(SEPARATOR);
  const sanitizeSeparators = replace(SEPARATOR)(delimiter);

  return pipe(sanitizeSeparators, splitCommas, toNumbers)(characters);
};

type Character = {
  delimiter: string,
  characters: string,
};
type ToCharacter = (input: string) => Character;
const toCharacter: ToCharacter = input => {
  const DELIMITER_PREFIX = '//';
  const NEW_LINE_DELIMITER = '\n';

  const createCharacter = delimiterIndex => {
    if (delimiterIndex === -1) {
      return {
        delimiter: NEW_LINE_DELIMITER,
        characters: input,
      };
    } else {
      const extractDelimiter = ([delimiter, newCharacters]) => {
        const [_, newDelimeter] = split(DELIMITER_PREFIX)(delimiter);
        return {
          delimiter: newDelimeter,
          characters: newCharacters,
        };
      };

      return pipe(
        split(NEW_LINE_DELIMITER),
        extractDelimiter
      )(input);
    }
  };
  const findDelimiterIndex = input => indexOf(DELIMITER_PREFIX)(input);
  const fillWithZeroIfEmpty = input => {
    if (input.length === '0') {
      return '0';
    } else {
      return input;
    }
  };

  return pipe(fillWithZeroIfEmpty, findDelimiterIndex, createCharacter)(input);
};

type AddNumbers = (input: string) => number;
const addNumbers: AddNumbers = input => {
  const BIG_NUMBERS = 1000;

  const filterBigNumbers = filter(number => number < BIG_NUMBERS);
  const arraySum = reduce((acc, number) => acc + number)(0);

  return pipe(
    toCharacter,
    toNumber,
    checkForNegativeNumbers,
    into(filterBigNumbers),
    into(arraySum),
    getValueOrFail
  )(input);
};

export { addNumbers as default };
