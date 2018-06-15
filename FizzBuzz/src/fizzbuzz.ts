/** @flow */
export const FIZZ = 'Fizz';
export const BUZZ = 'Buzz';
export const FIZZ_BUZZ = 'FizzBuzz';

type GenerateOutput = (listLength: number) => string;
const generateOutput: GenerateOutput = listLength => {
  throw new Error('Not implemented yet');
};

export default generateOutput;
