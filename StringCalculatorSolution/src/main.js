/** @flow */
import { pipe } from 'ramda';

import { getValueOrFail, into, Result } from './folktale';
import { findIndex, filter, map, reduce } from './array';
import { indexOf, split } from './string';

type CheckForNegativeNumbers = (array: number[]) => Result<string, number[]>;
const checkForNegativeNumbers: CheckForNegativeNumbers = array => {
  const isNumberNegative = number => number < 0;
  const negativeNumberIndex = findIndex(isNumberNegative);
  const createResult = negativeNumberIndex => {
    if (negativeNumberIndex === -1) {
      return Result.Ok(array);
    } else {
      return Result.Error('negatives not allowed');
    }
  };

  return pipe(negativeNumberIndex, createResult)(array);
};

opaque type Number = number[];
type ToNumber = (character: Character) => Number;
const toNumber: ToNumber = character => {
  const sanitizeSeparators = character => {
    const { delimeter, characters } = character;
    return characters.replace(delimeter, ',');
  };
  const toNumbers = map(characters => parseInt(characters));
  const splitCommas = split(',');

  return pipe(sanitizeSeparators, splitCommas, toNumbers)(character);
};

opaque type Character = {
  delimeter: string,
  characters: string,
};
type ToCharacter = (userInput: string) => Character;
const toCharacter: ToCharacter = userInput => {
  const DELIMITER = '//';
  const createCharacter = delimiterIndex => {
    switch (delimiterIndex) {
      case -1:
        return {
          delimeter: '\n',
          characters: userInput,
        };
      default: {
        const [delimeter, newCharacters] = split('\n')(userInput);
        return {
          delimeter: delimeter.charAt(2),
          characters: newCharacters,
        };
      }
    }
  };
  const findDelimiterIndex = userInput => indexOf(DELIMITER)(userInput);
  const fillWithZeroIfEmpty = userInput => {
    switch (userInput.length) {
      case '0':
        return '0';
      default:
        return userInput;
    }
  };

  return pipe(fillWithZeroIfEmpty, findDelimiterIndex, createCharacter)(
    userInput
  );
};

type AddNumbers = (characters: string) => number;
const addNumbers: AddNumbers = characters => {
  const BIG_NUMBERS = 1000;
  const filterBigNumbers = filter(number => number < BIG_NUMBERS);
  const arraySsum = reduce((acc, number) => acc + number)(0);

  return pipe(
    toCharacter,
    toNumber,
    checkForNegativeNumbers,
    into(filterBigNumbers),
    into(arraySsum),
    getValueOrFail
  )(characters);
};

export { addNumbers as default };
