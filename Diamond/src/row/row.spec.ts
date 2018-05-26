import createRow, { addRemainingRows } from './index';

describe('Row', () => {
  [
    { letter: 'A', row: ['A'], expectedRows: ['A'] },
    { letter: 'B', row: ['B B'], expectedRows: [' A ', 'B B', ' A '] },
    { letter: 'C', row: ['C   C'], expectedRows: ['  A  ', ' B B ', 'C   C', ' B B ', '  A  '] }
  ].forEach(({ letter, row, expectedRows }) => {
    it(`should have the shape "${expectedRows}" when the letter is "${letter}"`, () => {
      // when
      const newRows = addRemainingRows(letter)(row);

      // then
      expect(newRows).toEqual(expectedRows);
    });
  });

  [
    { letter: 'A', expectedRow: 'A' },
    { letter: 'B', expectedRow: 'B B' },
    { letter: 'C', expectedRow: 'C   C' }
  ].forEach(({ letter, expectedRow }) => {
    it(`should create "${expectedRow}" when the letter is "${letter}"`, () => {
      // when
      const diamond = createRow(letter);

      // then
      expect(diamond).toEqual(expectedRow);
    });
  });
});
