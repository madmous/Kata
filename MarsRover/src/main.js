/** @flow */

/**
 * DOMAIN
 */
export type Game = Array<'r' | 'o' | ' '>[];

export type Direction = 'N' | 'S' | 'E' | 'W';

export type Coordinate = {
  x: number,
  y: number,
};

export type Obstacles = Coordinate[];

export type Dimension = {
  width: number,
  height: number,
};

export type Rover = {
  coordinate: Coordinate,
  direction: Direction,
};

export type World = {
  rover: Rover,
  dimension: Dimension,
  obstacles: Obstacles,
};

export type MoveCommand = 'f' | 'b';
export type RotateCommand = 'l' | 'r';

export type Command = MoveCommand | RotateCommand;

/**
 * WORKFLOW
 */

export type NextRoverState = (
  world: World,
  command: Command
) => { coordinate: Coordinate } | { direction: Direction };

export type Execute = (world: World, commands: Command[]) => World;

export type MoveRover = (
  world: World,
  command: Command
) => { coordinate: Coordinate };

type NextCoordinateValue = (
  nextCoordinate: number,
  dimension: number
) => number;

type IsRoverOnObstacle = (
  coordinate: Coordinate,
  obstacles: Obstacles
) => boolean;

type RotateRover = (
  world: World,
  command: RotateCommand
) => { direction: Direction };

/**
 *
 */

const rotateRover: RotateRover = (world, command) => {
  const { rover: { direction, coordinate: { x, y } } } = world;
  const nextDirection = {
    N: {
      l: 'W',
      r: 'E',
    },
    S: {
      l: 'E',
      r: 'W',
    },
    E: {
      l: 'N',
      r: 'S',
    },
    W: {
      l: 'S',
      r: 'N',
    },
  }[direction][command];

  return {
    direction: nextDirection,
  };
};

const isRoverOnObstacle: IsRoverOnObstacle = ({ x, y }, obstacles) => {
  const sameCoordinate = obs => obs.x === x && obs.y === y;

  return obstacles.find(sameCoordinate) !== undefined;
};

const nextCoordinateValue: NextCoordinateValue = (nexPosition, dimension) =>
  (nexPosition + dimension) % dimension;

const moveRover: MoveRover = (world, command) => {
  const {
    dimension: { width, height },
    rover: { coordinate, direction },
    obstacles,
  } = world;
  const { x, y } = coordinate;
  const orientation = command === 'f' ? 1 : -1;
  const nextCoordinate = {
    N: () => ({
      x,
      y: nextCoordinateValue(y + orientation, height),
    }),
    S: () => ({
      x,
      y: nextCoordinateValue(y - orientation, height),
    }),
    E: () => ({
      x: nextCoordinateValue(x + orientation, width),
      y,
    }),
    W: () => ({
      x: nextCoordinateValue(x - orientation, width),
      y,
    }),
  }[direction]();

  if (isRoverOnObstacle(nextCoordinate, obstacles)) {
    return {
      coordinate,
    };
  }

  return {
    coordinate: nextCoordinate,
  };
};

const nextRoverState: NextRoverState = (world, command) =>
  ({
    f: () => moveRover(world, 'f'),
    b: () => moveRover(world, 'b'),
    l: () => rotateRover(world, 'l'),
    r: () => rotateRover(world, 'r'),
  }[command]());

export const execute: Execute = (world, commands) => {
  const nextWorldState = commands.reduce(
    (world, command) => ({
      ...world,
      rover: {
        ...world.rover,
        ...nextRoverState(world, command),
      },
    }),
    world
  );

  return nextWorldState;
};
