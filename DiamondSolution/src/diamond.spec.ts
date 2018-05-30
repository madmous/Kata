import {
  asciichar,
  bless,
  check,
  checkForall,
  forall,
  generator,
  property,
  random,
  record,
  shrink
} from 'jsverify';
import { isEqual } from 'lodash';
import {
  every,
  filter,
  flatMap,
  flow,
  head,
  includes,
  join,
  last,
  map,
  range,
  reverse,
  size,
  split,
  tail,
  take,
  takeRightWhile,
  takeWhile,
  trim,
  uniq
} from 'lodash/fp';

import makeDiamondWith, { createVerticalLettersFrom } from './diamond';

describe('DiamondProperty', () => {
  const checkOptions = {
    quiet: true,
    tests: 26
  };

  it('Non empty', () => {
    const prop = check(
      forall(arbLetter(), letter => {
        const diamond = makeDiamondWith(letter);

        return size(diamond) !== 0;
      }),
      checkOptions
    );

    expect(prop).toBe(true);
  });

  it('First row contains A', () => {
    const prop = check(
      forall(arbLetter(), letter => {
        const diamond = makeDiamondWith(letter);

        return flow(getFirstRow, trim)(diamond) === 'A';
      }),
      checkOptions
    );

    expect(prop).toBe(true);
  });

  it('Last row contains A', () => {
    const prop = check(
      forall(arbLetter(), letter => {
        const diamond = makeDiamondWith(letter);

        return flow(getLastRow, trim)(diamond) === 'A';
      }),
      checkOptions
    );

    expect(prop).toBe(true);
  });

  it('Rows must contain the correct letters, in the correct order', () => {
    const prop = check(
      forall(arbLetter(), letter => {
        const diamond = makeDiamondWith(letter);

        // not happy with the use of a function from production code
        const actual = createVerticalLettersFrom(letter);

        const expectToEqual = (actual: string[]) => (expected: string[]) =>
          isEqual(actual, expected);

        return flow(
          getRows,
          flatMap(keepUniqueLetters), // flatmap not typed correctly ??
          filter(isSpace),
          expectToEqual(actual)
        )(diamond);
      }),
      checkOptions
    );

    expect(prop).toBe(true);
  });

  it("Diamond is as wide as it's high", () => {
    const prop = check(
      forall(arbLetter(), letter => {
        const diamond = makeDiamondWith(letter);

        const rows = getRows(diamond);
        const expectRowLengthToEqualLengthOf = (rows: string[]) => (row: string) =>
          size(row) === size(rows);

        return every(expectRowLengthToEqualLengthOf(rows))(rows);
      }),
      checkOptions
    );

    expect(prop).toBe(true);
  });

  it('All rows except top and bottom have two identical letters', () => {
    const prop = check(
      forall(arbLetter(), letter => {
        const diamond = makeDiamondWith(letter);

        const expectLetterCountToBe2Per = (row: string) => size(removeSpaces(row)) === 2;

        return flow(getRows, removeHeadAndLast, every(expectLetterCountToBe2Per))(diamond);
      }),
      checkOptions
    );

    expect(prop).toBe(true);
  });

  it('Rows must have a symmetric contour', () => {
    const prop = check(
      forall(arbLetter(), letter => {
        const diamond = makeDiamondWith(letter);

        const expectSpacesToBeEqual = (row: string) =>
          isEqual(getLeadingSpaces(row), getTrailingSpaces(row));

        return flow(getRows, every(expectSpacesToBeEqual))(diamond);
      }),
      checkOptions
    );

    expect(prop).toBe(true);
  });

  it('Lower left space is a triangle', () => {
    const prop = check(
      forall(arbLetter(), letter => {
        const diamond = makeDiamondWith(letter);

        const expectLowerLeftSpacesToBeOrdered = (lowerLeftSpaces: number[]) =>
          isEqual(lowerLeftSpaces, range(1, size(lowerLeftSpaces) + 1));

        return flow(getLowerLeftSpaces(letter), expectLowerLeftSpacesToBeOrdered)(diamond);
      }),
      checkOptions
    );

    expect(prop).toBe(true);
  });
});

const arbLetter = () =>
  bless({
    generator: generator.bless(() => String.fromCharCode(random(65, 90))),
    show: val => val,
    shrink: shrink.noop
  });

const getRows = (diamond: string) => split('\n')(diamond);

const removeHeadAndLast = (rows: string[]) => take(rows.length - 2)(tail(rows));

const getFirstRow = (diamond: string) => head(getRows(diamond));

const getLastRow = (diamond: string) => last(getRows(diamond));

const removeSpaces = (row: string) => join('')(split(' ')(row));

const getLeadingSpaces = (row: string) => takeWhile((char: string) => char === ' ')(row);

const getTrailingSpaces = (row: string) => takeRightWhile((char: string) => char === ' ')(row);

const keepUniqueLetters = (row: string) => uniq(trim(row));

const isSpace = (v: string) => v !== ' ';

const getLowerLeftSpaces = (letter: string) => (diamond: string) =>
  flow(
    getRows,
    takeRightWhile((row: string) => {
      return !includes(letter)(row);
    }),
    map(getLeadingSpaces),
    map(size)
  )(diamond);
