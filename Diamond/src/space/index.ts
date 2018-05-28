import { range } from 'lodash';
import { flow, head, pad, padStart, sum } from 'lodash/fp';

import { Row } from '../row/index';

// TYPE(S)

type Space = string;

// MAIN

type IsA = (letter: string) => boolean;
const isA: IsA = letter => letter === 'A';

export default isA;

type CreateInnerSpacesFor = (letter: string) => Space;
export const createInnerSpacesFor: CreateInnerSpacesFor = letter => {
  if (isA(letter)) {
    return '';
  } else {
    return flow(findDiamondSizeFromLetter, flippedPadStart(''))(letter);
  }
};

type findDiamondSizeFromLetter = (letter: string) => number;
const findDiamondSizeFromLetter: findDiamondSizeFromLetter = letter =>
  flow(getAlphabetPosition, range, sum)(letter);

type GetAlphabetPosition = (letter: string) => number;
const getAlphabetPosition: GetAlphabetPosition = letter => letter.charCodeAt(0) - 64;

type AddOuterSpaces = (rows: Row[]) => (row: Row) => Row;
export const addOuterSpaces: AddOuterSpaces = rows => row =>
  flow(findDiamondSizeFromRows, flippedPad(row))(rows);

// UTILS

type FlippedPadStart = (char: string) => (length: number) => string;
const flippedPadStart: FlippedPadStart = char => length => padStart(length)(char);

type FlippedPad = (char: string) => (length: number) => string;
const flippedPad: FlippedPad = char => length => pad(length)(char);

type findDiamondSizeFromRows = (rows: Row[]) => number;
const findDiamondSizeFromRows: findDiamondSizeFromRows = rows => head(rows).length;
