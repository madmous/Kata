import { range } from 'lodash';
import {
  concat,
  filter,
  flow,
  head,
  initial,
  join,
  map,
  nth,
  pad,
  range as curriedRange,
  repeat,
  reverse,
  size,
  tail,
  zip
} from 'lodash/fp';

type MakeDiamondWith = (letter: string) => string;
const makeDiamondWith: MakeDiamondWith = letter => {
  if (letter === 'A') {
    return 'A';
  } else {
    const letters = enumerateLettersUpTo(letter);
    const verticalLines = [...letters, ...reverse(initial(letters))];
    return flow(
      map(toRow(size(verticalLines))),
      join('\n')
    )(verticalLines);
  }
};

export default makeDiamondWith;

type EnumerateLettersUpTo = (letter: string) => string[];
const enumerateLettersUpTo: EnumerateLettersUpTo = letter => {
  const toString = (code: number) => String.fromCharCode(code);
  const charCodesUpToLetter = curriedRange(65)(letter.charCodeAt(0) + 1);
  return map(toString)(charCodesUpToLetter);
};

type ToRow = (width: number) => (letter: string) => string;
const toRow: ToRow = width => letter => {
  if (letter === 'A') {
    return pad(width)('A');
  } else {
    const withInnerSpaces = letter + repeat(countInnerSpaces(letter))(' ') + letter;
    return pad(width)(withInnerSpaces);
  }
};

type CountInnerSpaces = (letter: string) => number;
const countInnerSpaces: CountInnerSpaces = letter => {
  const isLetterCharCodeIndex = ([alphabetIndex, _]: [number, number]) =>
    alphabetIndex === letter.charCodeAt(0) - 65;
  const innerSpacesByIndex = zip(range(0, 26))([0, ...range(1, 51, 2)]);
  return flow(
    filter(isLetterCharCodeIndex),
    head,
    nth(1)
  )(innerSpacesByIndex);
};

// TEST

type CreateVerticalLettersFrom = (letter: string) => string[];
export const createVerticalLettersFrom: CreateVerticalLettersFrom = (letter: string) =>
  flow(
    enumerateLettersUpTo,
    createSymetry
  )(letter);

type CreateSymetry = (rows: string[]) => string[];
const createSymetry: CreateSymetry = rows =>
  flow(
    reverse,
    tail,
    concat(rows)
  )(rows);
