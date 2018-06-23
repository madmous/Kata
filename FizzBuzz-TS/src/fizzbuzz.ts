/** @flow */
import { flow, join, map, range } from 'lodash/fp';

export const FIZZ = 'Fizz';
export const BUZZ = 'Buzz';
export const FIZZ_BUZZ = 'FizzBuzz';

type GenerateOutput = (listLength: number) => string;
const generateOutput: GenerateOutput = listLength => {
  return flow(
    range(1),
    map(inputToWordIfNeeded),
    join('\n')
  )(listLength + 1);
};

const inputToWordIfNeeded = (input: number) => {
  if (divisibleBy3And5(input)) {
    return FIZZ_BUZZ;
  } else if (divisibleBy(3)(input)) {
    return FIZZ;
  } else if (divisibleBy(5)(input)) {
    return BUZZ;
  } else {
    return input;
  }
};

type DivisibleBy = (num: number) => (value: number) => boolean;
const divisibleBy: DivisibleBy = num => value => value % num === 0;

type DivisibleBy3And5 = (value: number) => boolean;
export const divisibleBy3And5: DivisibleBy3And5 = value => value % 3 === 0 && value % 5 === 0;

export default generateOutput;
