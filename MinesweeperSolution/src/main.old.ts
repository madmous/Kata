import {
  add,
  concat,
  differenceWith,
  drop,
  filter,
  flow,
  head,
  isEqual,
  join,
  map,
  reduce,
  split,
  subtract,
  tail,
  take,
} from 'lodash/fp';

import { adjust, update } from 'ramda';

import { flatten } from './utils';

import log from './log';

export type InputField = ReadonlyArray<ReadonlyArray<string>>;
export type Field = ReadonlyArray<ReadonlyArray<number>>;

interface FieldBounds {
  width: number;
  height: number;
}

export interface Coordinate {
  x: number;
  y: number;
}

interface Position {
  column: number;
  row: number;
}

type ToString = (field: Field) => string;
const toString: ToString = field => flow(map(join('')), join(' '))(field);

type ToNextField = (field: Field) => (coordinate: Coordinate) => Field;
const toNextField: ToNextField = field => coordinate => {
  const { x, y } = coordinate;
  const nextRow = adjust(add(1), y, field[x]);

  return update(x, nextRow, field);
};

type CountAdjacentMinesOn = (
  field: Field
) => (coordinates: ReadonlyArray<Coordinate>) => ReadonlyArray<number>;
const countAdjacentMinesOn: CountAdjacentMinesOn = field => coordinates => {
  if (coordinates.length === 0) {
    return flatten(field);
  } else {
    const coordinate = head(coordinates);
    const nextCoordinates = tail(coordinates);
    const nextField = toNextField(field)(coordinate);

    return countAdjacentMinesOn(nextField)(nextCoordinates);
  }
};

type FilterCoordinatesEqualTo = (
  mines: ReadonlyArray<Coordinate>
) => (adjacentSquares: ReadonlyArray<Coordinate>) => ReadonlyArray<Coordinate>;
const filterCoordinatesEqualTo: FilterCoordinatesEqualTo = mines => adjacentSquares => {
  return differenceWith(isEqual, adjacentSquares, mines);
};

type IsCoordinateOutOf = (
  filedBounds: FieldBounds
) => (coordinates: Coordinate) => boolean;
const isCoordinateOutOf: IsCoordinateOutOf = filedBounds => minePosition =>
  minePosition.x >= 0 &&
  minePosition.x < filedBounds.height &&
  minePosition.y >= 0 &&
  minePosition.y < filedBounds.width;

type FilterCoordinatesOutOf = (
  fieldBounds: FieldBounds
) => (coordinates: ReadonlyArray<Coordinate>) => ReadonlyArray<Coordinate>;
const filterCoordinatesOutOf: FilterCoordinatesOutOf = fieldBounds => minePositions =>
  filter(isCoordinateOutOf(fieldBounds))(minePositions);

type CalculateAdjacentCoordinateToMine = (
  coordinate: Coordinate
) => ReadonlyArray<Coordinate>;
const calculateAdjacentCoordinateToMine: CalculateAdjacentCoordinateToMine = coordinate => {
  const { x, y } = coordinate;
  const top = { x, y: add(1)(y) };
  const bottom = { x, y: subtract(y)(1) };
  const left = { x: subtract(x)(1), y };
  const right = { x: add(1)(x), y };

  const topLeft = {
    x: subtract(x)(1),
    y: add(1)(y),
  };
  const topRight = {
    x: add(1)(x),
    y: add(1)(y),
  };
  const bottomLeft = {
    x: subtract(x)(1),
    y: subtract(y)(1),
  };
  const bottomRight = {
    x: add(1)(x),
    y: subtract(y)(1),
  };

  return [top, bottom, left, right, topLeft, topRight, bottomLeft, bottomRight];
};

type CalculateAdjacentCoordinatesToMine = (
  coordinates: ReadonlyArray<Coordinate>
) => ReadonlyArray<Coordinate>;
const calculateAdjacentCoordinatesToMine: CalculateAdjacentCoordinatesToMine = coordinates =>
  flow(map(calculateAdjacentCoordinateToMine), flatten)(coordinates);

type GetNextPosition = (character: string) => (position: Position) => Position;
const getNextPosition: GetNextPosition = character => position => {
  const { row, column } = position;
  if (character === ' ') {
    return { column: 0, row: add(1)(row) };
  } else {
    return { column: add(1)(column), row };
  }
};

type GetNextCoordinates = (
  coordinates: ReadonlyArray<Coordinate>
) => (character: string) => (position: Position) => ReadonlyArray<Coordinate>;
const getNextCoordinates: GetNextCoordinates = coordinates => character => position => {
  const { row, column } = position;
  if (character === '*') {
    return [...coordinates, { x: row, y: column }];
  } else {
    return coordinates;
  }
};

type FindMineCoordinates = (
  accumulutator: ReadonlyArray<Coordinate>
) => (position: Position) => (input: string) => ReadonlyArray<Coordinate>;
const findMineCoordinates: FindMineCoordinates = accumulutator => position => input => {
  if (input.length === 0) {
    return accumulutator;
  } else {
    const firstChar = head(input);
    const nextCoordinates = getNextCoordinates(accumulutator)(firstChar)(
      position
    );
    const nextPos = getNextPosition(firstChar)(position);
    const nextInput = input.substring(1);

    return findMineCoordinates(nextCoordinates)(nextPos)(nextInput);
  }
};

type placeMineCharacterIfNcessaryOn = (field: number) => number | string;
const placeMineCharacterIfNcessaryOn: placeMineCharacterIfNcessaryOn = value => {
  if (value === -1) {
    return '*';
  } else {
    return value;
  }
};

type placeMineCharacterOnField = (
  field: ReadonlyArray<number>
) => ReadonlyArray<number | string>;
const placeMineCharacterOnField: placeMineCharacterOnField = field =>
  map(placeMineCharacterIfNcessaryOn)(field);

type ToField = (
  width: number
) => (accumulator: Field) => (fllatennedField: ReadonlyArray<number>) => Field;
const toField: ToField = width => accumulator => fllatennedField => {
  const acc = [...accumulator];
  if (fllatennedField.length === 0) {
    return accumulator;
  } else {
    const nextField = concat(acc)([take(width)(fllatennedField)]);
    const nextFllatennedField = drop(width)(fllatennedField);

    return toField(width)(nextField)(nextFllatennedField);
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

type ReplaceDotsWithZeros = (inputField: InputField) => ReadonlyArray<number>;
const replaceDotsWithZeros: ReplaceDotsWithZeros = inputField =>
  map(replaceDotsWithZerosIfNecessary)(flatten(inputField));

type GetFieldBounds = (inputField: InputField) => FieldBounds;
const getFieldBounds: GetFieldBounds = inputField => ({
  height: inputField.length,
  width: head(inputField).length,
});

type ToInputField = (input: string) => InputField;
const toInputField: ToInputField = input =>
  flow(split(' '), map(split('')))(input);

type FindMines = (input: string) => string;
const findMines: FindMines = input => {
  const inputField = toInputField(input);
  const fieldBounds = getFieldBounds(inputField);

  const field = flow(replaceDotsWithZeros, toField(fieldBounds.width)([]))(
    inputField
  );

  const mines = findMineCoordinates([])({ row: 0, column: 0 })(input);

  return flow(
    calculateAdjacentCoordinatesToMine,
    filterCoordinatesOutOf(fieldBounds),
    filterCoordinatesEqualTo(mines),
    countAdjacentMinesOn(field),
    placeMineCharacterOnField,
    toField(fieldBounds.width)([]),
    toString
  )(mines);
};

export { findMines as default };
