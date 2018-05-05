import {
  add,
  clone,
  concat,
  drop,
  filter,
  flatten,
  flow,
  forEach,
  head,
  join,
  map,
  split,
  subtract,
  take,
} from 'lodash/fp';

import log from './log';

type FieldInString = string[][];
type FieldInNumber = number[][];
type Field = Array<Array<number | string>>;

interface FieldDimension {
  width: number;
  height: number;
}

interface Coordinate {
  x: number;
  y: number;
}

interface Position {
  column: number;
  row: number;
}

type ToString = (matrix: FieldInString) => string;
const toString: ToString = matrix => flow(map(join('')), join(' '))(matrix);

type FillMines = (
  matrix: FieldInNumber
) => (coordinates: Coordinate[]) => FieldInNumber;
const fillMines: FillMines = matrix => coordinates => {
  const newMatrix = clone(matrix);

  forEach((coordinate: Coordinate) => {
    const value = newMatrix[coordinate.x][coordinate.y];

    if (value !== -1) {
      newMatrix[coordinate.x][coordinate.y] = add(1)(
        newMatrix[coordinate.x][coordinate.y]
      );
    }
  })(coordinates);

  return newMatrix;
};

type RemoveImpossibleValues = (
  fieldDimension: FieldDimension
) => (coordinates: Coordinate[]) => Coordinate[];
const removeImpossibleValues: RemoveImpossibleValues = fieldDimension => minePositions =>
  filter(
    (minePosition: Coordinate) =>
      minePosition.x >= 0 &&
      minePosition.x < fieldDimension.height &&
      minePosition.y >= 0 &&
      minePosition.y < fieldDimension.width
  )(minePositions);

type CalculatePotentialPositionsAroundAMine = (
  mineCoordinates: Coordinate[]
) => Coordinate[][];
const calculatePotentialPositionsAroundAMine: CalculatePotentialPositionsAroundAMine = mineCoordinates => {
  return map((mineCoordinate: Coordinate) => {
    const top = { x: mineCoordinate.x, y: add(1)(mineCoordinate.y) };
    const bottom = { x: mineCoordinate.x, y: subtract(mineCoordinate.y)(1) };
    const left = { x: subtract(mineCoordinate.x)(1), y: mineCoordinate.y };
    const right = { x: add(1)(mineCoordinate.x), y: mineCoordinate.y };

    const topLeft = {
      x: subtract(mineCoordinate.x)(1),
      y: add(1)(mineCoordinate.y),
    };
    const topRight = {
      x: add(1)(mineCoordinate.x),
      y: add(1)(mineCoordinate.y),
    };
    const bottomLeft = {
      x: subtract(mineCoordinate.x)(1),
      y: subtract(mineCoordinate.y)(1),
    };
    const bottomRight = {
      x: add(1)(mineCoordinate.x),
      y: subtract(mineCoordinate.y)(1),
    };

    return [
      top,
      bottom,
      left,
      right,
      topLeft,
      topRight,
      bottomLeft,
      bottomRight,
    ];
  })(mineCoordinates);
};

type GetNextPosition = (character: string) => (pos: Position) => Position;
const getNextPosition: GetNextPosition = character => pos => {
  const { row, column } = pos;
  if (character === ' ') {
    return { column: 0, row: add(1)(row) };
  } else {
    return { column: add(1)(column), row };
  }
};

type GetMineCoordinates = (
  coordinates: Coordinate[]
) => (character: string) => (pos: Position) => Coordinate[];
const getNextCoordinates: GetMineCoordinates = coordinates => character => pos => {
  if (character === '*') {
    return [...coordinates, { x: pos.row, y: pos.column }];
  } else {
    return coordinates;
  }
};

type FindMineCoordinates = (
  coordinates: Coordinate[]
) => (pos: Position) => (input: string) => Coordinate[];
const findMineCoordinates: FindMineCoordinates = coordinates => pos => input => {
  if (input.length === 0) {
    return coordinates;
  } else {
    const firstChar = head(input);

    const nextCoordinates = getNextCoordinates(coordinates)(firstChar)(pos);
    const nextPos = getNextPosition(firstChar)(pos);
    const nextInput = input.substring(1);

    return findMineCoordinates(nextCoordinates)(nextPos)(nextInput);
  }
};

type PlaceMinesIfNecessary = (field: number) => number | string;
const placeMinesIfNecessary: PlaceMinesIfNecessary = value => {
  if (value === -1) {
    return '*';
  } else {
    return value;
  }
};

type PlaceMines = (field: number[]) => Array<number | string>;
const placeMines: PlaceMines = field => map(placeMinesIfNecessary)(field);

type RestoreField = (
  width: number
) => (matrix: FieldInNumber) => (fllatennedMatrix: number[]) => FieldInNumber;
const restoreField: RestoreField = width => matrix => fllatennedMatrix => {
  if (fllatennedMatrix.length === 0) {
    return matrix;
  } else {
    const nextMatrix = concat(matrix)([take(width)(fllatennedMatrix)]);
    const nextFllatennedMatrix = drop(width)(fllatennedMatrix);

    return restoreField(width)(nextMatrix)(nextFllatennedMatrix);
  }
};

type ReplaceDotsWithZerosIfNecessary = (input: string) => number;
const replaceDotsWithZerosIfNecessary: ReplaceDotsWithZerosIfNecessary = input => {
  if (input === '.') {
    return 0;
  } else {
    return -1;
  }
};

type ReplaceDotsWithZeros = (input: string[]) => number[];
const replaceDotsWithZeros: ReplaceDotsWithZeros = input =>
  map(replaceDotsWithZerosIfNecessary)(input);

type GetFieldDimension = (matrix: FieldInString) => FieldDimension;
const getFieldDimension: GetFieldDimension = matrix => ({
  height: matrix.length,
  width: head(matrix).length,
});

type ToMatrix = (input: string) => FieldInString;
const toMatrix: ToMatrix = input => flow(split(' '), map(split('')))(input);

type FindMines = (input: string) => string;
const findMines: FindMines = input => {
  const matrix = toMatrix(input);
  const fieldDimension = getFieldDimension(matrix);

  const filledMatrix = flow(
    flatten,
    replaceDotsWithZeros,
    restoreField(fieldDimension.width)([])
  )(matrix);

  return flow(
    findMineCoordinates([])({ row: 0, column: 0 }),
    calculatePotentialPositionsAroundAMine,
    flatten,
    removeImpossibleValues(fieldDimension),
    fillMines(filledMatrix),
    flatten,
    placeMines,
    restoreField(fieldDimension.width)([]),
    toString
  )(input);
};

export { findMines as default };
