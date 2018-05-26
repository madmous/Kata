import createInnerSpaces from './index';

describe('Space', () => {
  [
    { letter: 'A', expectedDiamond: '' },
    { letter: 'B', expectedDiamond: ' ' },
    { letter: 'C', expectedDiamond: '   ' }
  ].forEach(({ letter, expectedDiamond }) => {
    it(`should create "[${
      expectedDiamond.length
    }]" internal spaces when the letter is "${letter}"`, () => {
      // when
      const diamond = createInnerSpaces(letter);

      // then
      expect(diamond).toEqual(expectedDiamond);
    });
  });
});
