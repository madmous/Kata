import { add, concat, flow, join, map, pad, range, reverse, tail } from 'lodash/fp';

type Diamond = string;

type MakeDiamondWith = (letter: string) => Diamond;
const makeDiamondWith: MakeDiamondWith = letter => {
  if (letter === 'A') {
    return 'A';
  } else {
    const letters = createVerticalLettersFrom(letter);

    return flow(map(createRow(letters.length)), join('\n'))(letters);
  }
};

export default makeDiamondWith;

type CreateVerticalLettersFrom = (letter: string) => string[];
export const createVerticalLettersFrom: CreateVerticalLettersFrom = (letter: string) =>
  flow(createCharactersUpto, map(fromCharCode), createSymetry)(letter);

type CreateCharactersUpto = (letter: string) => number[];
const createCharactersUpto: CreateCharactersUpto = letter =>
  flow(charCodeAt, add(1), range(65))(letter);

type CreateSymetry = (rows: string[]) => string[];
const createSymetry: CreateSymetry = rows => flow(reverse, tail, concat(rows))(rows);

type CreateRow = (spaces: number) => (letter: string) => string;
const createRow: CreateRow = spaces => letter => {
  if (letter === 'A') {
    return pad(spaces)('A');
  } else {
    return pad(spaces)(letter + createInnerSpaces(letter) + letter);
  }
};

type CreateInnerSpaces = (letter: string) => string;
const createInnerSpaces: CreateInnerSpaces = letter =>
  flow(charCodeAt, add(-65), calculateSpaces, flippedPad(' '))(letter);

type CalculateSpaces = (index: number) => number;
const calculateSpaces: CalculateSpaces = index => {
  const ALPHABET_LETTER_COUNT = 26;
  const letterSpaces = range(0)(ALPHABET_LETTER_COUNT);

  return letterSpaces[index] + letterSpaces[index - 1];
};

// UTILS

const fromCharCode = (code: number) => String.fromCharCode(code);

const charCodeAt = (letter: string) => letter.charCodeAt(0);

const flippedPad = (char: string) => (length: number) => pad(length)(char);
