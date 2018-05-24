/** @flow */

export type RotateCommand = 'l' | 'r';
export type MoveCommand = 'f' | 'b';

export type Command = MoveCommand | RotateCommand;

export type Coordinate = {
  x: number,
  y: number,
};

export type Obstacles = Coordinate[];

export type Dimension = {
  width: number,
  height: number,
};

export type Direction = 'N' | 'S' | 'E' | 'W';

export type Rover = {
  coordinate: Coordinate,
  direction: Direction,
};

export type World = {
  rover: Rover,
  dimension: Dimension,
  obstacles: Obstacles,
};

export type Execute = (world: World, commands: Command[]) => World;
export const execute: Execute = (world, commands) => {
  throw new Error('Not implemented yet');
};
