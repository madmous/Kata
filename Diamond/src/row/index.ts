import { flow } from 'lodash/fp';

import isA, { addOuterSpaces, createInnerSpacesFor } from '../space/index';

// TYPE(S)

export type Row = string;

// MAIN

type CreateRow = (letter: string) => Row;
const createRow: CreateRow = letter => {
  if (isA(letter)) {
    return letter;
  } else {
    return letter + createInnerSpacesFor(letter) + letter;
  }
};

export default createRow;

// REST

type AddRemainingRows = (letter: string) => (rows: Row[]) => Row[];
export const addRemainingRows: AddRemainingRows = letter => rows => {
  if (isA(letter)) {
    return rows;
  } else {
    const nextLetter = getPrevious(letter);
    const nextRows = toNextRows(nextLetter)(rows);

    return addRemainingRows(nextLetter)(nextRows);
  }
};

type GetPrevious = (letter: string) => string;
const getPrevious: GetPrevious = letter => String.fromCharCode(letter.charCodeAt(0) - 1);

type ToNextRows = (letter: string) => (rows: Row[]) => Row[];
const toNextRows: ToNextRows = letter => rows => {
  const row = flow(createRow, addOuterSpaces(rows))(letter);

  return [row, ...rows, row];
};
