import { range } from 'lodash';
import { concat, drop, fill, filter, flow, head, join, pad, sum } from 'lodash/fp';

import isA, { getPreviousLetterFrom } from '../letter/index';
import createInnerSpaces from '../space/index';

// TYPE(S)

export type Row = string;

// MAIN

type CreateRow = (letter: string) => Row;
const createRow: CreateRow = letter => {
  if (isA(letter)) {
    return letter;
  } else {
    return letter + createInnerSpaces(letter) + letter;
  }
};

export default createRow;

// REST

type AddRemainingRows = (letter: string) => (rows: Row[]) => Row[];
export const addRemainingRows: AddRemainingRows = letter => rows => {
  if (isA(letter)) {
    return rows;
  } else {
    const nextLetter = getPreviousLetterFrom(letter);
    const nextRows = toNextRows(nextLetter)(rows);

    return addRemainingRows(nextLetter)(nextRows);
  }
};

type ToNextRows = (letter: string) => (rows: Row[]) => Row[];
const toNextRows: ToNextRows = letter => rows => {
  const row = flow(createRow, addOuterSpaces(rows))(letter);

  return [row, ...rows, row];
};

type AddOuterSpaces = (rows: Row[]) => (row: Row) => Row;
const addOuterSpaces: AddOuterSpaces = rows => row => {
  const rowLength = head(rows).length;

  return pad(rowLength)(row);
};
