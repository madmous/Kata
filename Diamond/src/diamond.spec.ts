import createDiamond from './diamond';

describe('Diamond', () => {
  [
    { letter: 'A', expectedDiamond: 'A' },
    { letter: 'B', expectedDiamond: ' A \nB B\n A ' },
    { letter: 'C', expectedDiamond: '  A  \n B B \nC   C\n B B \n  A  ' }
  ].forEach(({ letter, expectedDiamond }) => {
    it(`should have the shape "${expectedDiamond}" when the letter is "${letter}"`, () => {
      // when
      const diamond = createDiamond(letter);

      // then
      expect(diamond).toEqual(expectedDiamond);
    });
  });
});
