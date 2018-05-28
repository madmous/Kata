import createRow, { addRemainingRows } from './index';

// TODO: find a better property test name ? Is this describe necessary ?
describe('Next rows created from previous rows', () => {
  [
    { letter: 'A', row: ['A'], expectedRows: ['A'] },
    { letter: 'B', row: ['B B'], expectedRows: [' A ', 'B B', ' A '] },
    { letter: 'C', row: ['C   C'], expectedRows: ['  A  ', ' B B ', 'C   C', ' B B ', '  A  '] }
  ].forEach(({ letter, row, expectedRows }) => {
    it(`should be "${expectedRows}" when the letter is "${letter}"`, () => {
      // when
      const newRows = addRemainingRows(letter)(row);

      // then
      expect(newRows).toEqual(expectedRows);
    });
  });
});

describe('Row base', () => {
  [
    { letter: 'A', expectedRow: 'A' },
    { letter: 'B', expectedRow: 'B B' },
    { letter: 'C', expectedRow: 'C   C' }
  ].forEach(({ letter, expectedRow }) => {
    it(`should be "${expectedRow}" when the letter is "${letter}"`, () => {
      // when
      const diamond = createRow(letter);

      // then
      expect(diamond).toEqual(expectedRow);
    });
  });
});
