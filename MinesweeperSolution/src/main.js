/** @flow */

import { dec, findIndex, inc, map, pipe, split, without } from 'ramda';

import { Maybe } from './folktale';

type Position = {
  x: number,
  y: number,
}

type Matrix = string[][];

type FindPositionsAroundAMine = (matrix: Matrix) => (minePosition: Position) => Position[];
const findPositionsAroundAMine: FindPositionsAroundAMine = matrix => minePosition => {
  const up = matrix[minePosition.x][inc(minePosition.y)];
  const down = matrix[minePosition.x][dec(minePosition.y)];
  const left = matrix[dec(minePosition.x)][minePosition.y];
  const right = matrix[inc(minePosition.x)][minePosition.y];

  return without(undefined)[up, down, left, right];
};

type FindMineIndex = (horizontalLine: string[]) => Maybe<string>;
const findMineIndex: FindMineIndex = horizontalLine => {
  const indexFound = findIndex(v => v === '*')(horizontalLine);

  if (indexFound === -1) {
    return Maybe.Nothing();
  } else {
    return Maybe.Just(indexFound);
  }
};

type ToMatrix = (input: string) => Matrix;
const toMatrix: ToMatrix = input => pipe(split(' '), map(split('')))(input);

type FindMines = (input: string) => string;
const findMines: FindMines = input => {
  return '*100 2210 1*10 1110';
};

export { findMines as default };
