type IsA = (letter: string) => boolean;
const isA: IsA = letter => letter === 'A';

type GetPreviousLetterFrom = (letter: string) => string;
export const getPreviousLetterFrom: GetPreviousLetterFrom = letter =>
  String.fromCharCode(letter.charCodeAt(0) - 1);

type GetAlphabetPosition = (letter: string) => number;
export const getAlphabetPosition: GetAlphabetPosition = letter => letter.charCodeAt(0) - 64;

export default isA;
