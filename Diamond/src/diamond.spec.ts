import { head, last, split, trim } from 'lodash/fp';

import createDiamond from './diamond';

describe('Diamond top row ', () => {
  [{ letter: 'A' }, { letter: 'B' }, { letter: 'C' }].forEach(({ letter }) => {
    it('should always be "A"', () => {
      // when
      const diamond = createDiamond(letter);

      // then
      const firstLetter = trim(head(split('\n')(diamond)));
      expect(firstLetter).toEqual('A');
    });
  });
});

describe('Diamond bottom row ', () => {
  [{ letter: 'A' }, { letter: 'B' }, { letter: 'C' }].forEach(({ letter }) => {
    it('should always be "A"', () => {
      // when
      const diamond = createDiamond(letter);

      // then
      const firstLetter = trim(last(split('\n')(diamond)));
      expect(firstLetter).toEqual('A');
    });
  });
});

describe('Diamond shape', () => {
  [
    { letter: 'A', expectedDiamond: 'A' },
    { letter: 'B', expectedDiamond: ' A \nB B\n A ' },
    { letter: 'C', expectedDiamond: '  A  \n B B \nC   C\n B B \n  A  ' }
  ].forEach(({ letter, expectedDiamond }) => {
    it(`should be "${expectedDiamond}" when the letter is "${letter}"`, () => {
      // when
      const diamond = createDiamond(letter);

      // then
      expect(diamond).toEqual(expectedDiamond);
    });
  });
});
