import { getPreviousLetterFrom } from './index';

describe('Letter', () => {
  [
    { letter: 'B', expectedLetter: 'A'},
    { letter: 'C', expectedLetter: 'B'},
    { letter: 'D', expectedLetter: 'C'}
  ].forEach(({ letter, expectedLetter }) => {
    it(`should be preceded by "${expectedLetter}" when the letter is "${letter}"`, () => {
      // when
      const previousLetter = getPreviousLetterFrom(letter);

      // then
      expect(previousLetter).toEqual(expectedLetter);
    });
  });
});
