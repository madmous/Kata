import {
  add,
  concat,
  differenceWith,
  drop,
  filter,
  flatten,
  flow,
  head,
  isEqual,
  join,
  map,
  split,
  subtract,
  tail,
  take,
} from 'lodash/fp';

import { fromJS } from 'immutable';

import log from './log';

type InputField = ReadonlyArray<ReadonlyArray<string>>;
type Field = ReadonlyArray<ReadonlyArray<number>>;

interface FieldBounds {
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

type ToString = (field: Field) => string;
const toString: ToString = field => flow(map(join('')), join(' '))(field);

type ToNextField = (field: Field) => (coordinate: Coordinate) => Field;
const toNextField: ToNextField = field => coordinate => {
  const { x, y } = coordinate;
  const row = field[x];
  const nextRowList = fromJS(row).set(y, add(1)(row[y]));

  return fromJS(field)
    .set(x, nextRowList.toJS())
    .toJS();
};

type CountAdjacentMinesOn = (
  field: Field
) => (coordinates: Coordinate[]) => ReadonlyArray<number>;
const countAdjacentMinesOn: CountAdjacentMinesOn = field => coordinates => {
  if (coordinates.length === 0) {
    return fromJS(field)
      .flatten()
      .toJS();
  } else {
    const coordinate = head(coordinates);
    const nextCoordinates = tail(coordinates);
    const nextField = toNextField(field)(coordinate);

    return countAdjacentMinesOn(nextField)(nextCoordinates);
  }
};

type FilterCoordinatesEqualTo = (
  mines: Coordinate[]
) => (adjacentSquares: Coordinate[]) => Coordinate[];
const filterCoordinatesEqualTo: FilterCoordinatesEqualTo = mines => adjacentSquares =>
  differenceWith(isEqual)(adjacentSquares)(mines);

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
) => (coordinates: Coordinate[]) => Coordinate[];
const filterCoordinatesOutOf: FilterCoordinatesOutOf = fieldBounds => minePositions =>
  filter(isCoordinateOutOf(fieldBounds))(minePositions);

type CalculateAdjacentCoordinateToMine = (
  coordinate: Coordinate
) => Coordinate[];
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
  coordinates: Coordinate[]
) => Coordinate[];
const calculateAdjacentCoordinatesToMine: CalculateAdjacentCoordinatesToMine = coordinates =>
  flow(map(calculateAdjacentCoordinateToMine), flatten)(coordinates);

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
  accumulutator: Coordinate[]
) => (position: Position) => (input: string) => Coordinate[];
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

type placeMineCharacterOnField = (field: number[]) => Array<number | string>;
const placeMineCharacterOnField: placeMineCharacterOnField = field =>
  map(placeMineCharacterIfNcessaryOn)(field);

type ToField = (
  width: number
) => (accumulator: Field) => (fllatennedMatrix: number[]) => Field;
const toField: ToField = width => accumulator => fllatennedMatrix => {
  const acc = [...accumulator];
  if (fllatennedMatrix.length === 0) {
    return accumulator;
  } else {
    const nextMatrix = concat(acc)([take(width)(fllatennedMatrix)]);
    const nextFllatennedMatrix = drop(width)(fllatennedMatrix);

    return toField(width)(nextMatrix)(nextFllatennedMatrix);
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

type ReplaceDotsWithZeros = (inputField: InputField) => number[];
const replaceDotsWithZeros: ReplaceDotsWithZeros = inputField => {
  const flattened = fromJS(inputField)
    .flatten()
    .toJS();

  return map(replaceDotsWithZerosIfNecessary)(flattened);
};

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
