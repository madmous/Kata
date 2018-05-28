import { addOuterSpaces, createInnerSpacesFor } from './index';

describe('Inner spaces', () => {
  [
    { letter: 'A', expectedDiamond: '' },
    { letter: 'B', expectedDiamond: ' ' },
    { letter: 'C', expectedDiamond: '   ' }
  ].forEach(({ letter, expectedDiamond }) => {
    it(`should be "[${expectedDiamond.length}]" when the letter is "${letter}"`, () => {
      // when
      const diamond = createInnerSpacesFor(letter);

      // then
      expect(diamond).toEqual(expectedDiamond);
    });
  });
});

describe('Outer spaces', () => {
  [
    { rows: ['B B'], row: 'A', expectedRows: ' A ' },
    { rows: ['C   C'], row: 'B B', expectedRows: ' B B ' },
    { rows: ['C   C'], row: 'A', expectedRows: '  A  ' }
  ].forEach(({ rows, row, expectedRows }) => {
    it(`should be "[${expectedRows.length}]" when the letter is "${rows}"`, () => {
      // when
      const newRows = addOuterSpaces(rows)(row);

      // then
      expect(newRows).toEqual(expectedRows);
    });
  });
});
