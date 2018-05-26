import { range } from 'lodash';
import { flip, flow, padStart, sum } from 'lodash/fp';

import isA, { getAlphabetPosition } from '../letter/index';

// TYPE(S)

type Space = string;

// MAIN

type CreateInnerSpaces = (letter: string) => Space;
const createInnerSpaces: CreateInnerSpaces = letter => {
  if (isA(letter)) {
    return '';
  } else {
    return flow(getDiamondSize, flippedPadStart(''))(letter);
  }
};

export default createInnerSpaces;

// REST

type GetDiamondSize = (letter: string) => number;
const getDiamondSize: GetDiamondSize = letter => {
  if (isA(letter)) {
    return 1;
  } else {
    return flow(getAlphabetPosition, range, sum)(letter);
  }
};

type FlippedPadStart = (char: string) => (length: number) => string;
const flippedPadStart: FlippedPadStart = char => length => padStart(length)(char);
