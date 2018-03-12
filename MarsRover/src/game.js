/** @flow */

import type {
  Obstacles,
  Coordinate,
  RotateCommand,
  MoveCommand,
  Direction,
  Command,
  World,
  Game,
  Rover,
} from './main';

export function play(world: World): Game {
  const { dimension, obstacles, rover } = world;
  const { height, width } = dimension;
  const emptyGame = Array.from({ length: height }, e => Array(width).fill(' '));
  const gameWithRover = addRoverToGame(emptyGame, rover);
  const game = addObstaclesToGame(gameWithRover, obstacles);

  return game;
}

function addRoverToGame(game: Game, rover: Rover): Game {
  return addToGame('r', game, rover.coordinate);
}

function addObstaclesToGame(game: Game, obstacles: Obstacles): Game {
  const nextGameState = obstacles.reduce((game, obs) => {
    return addToGame('o', game, obs);
  }, game);

  return nextGameState;
}

function addToGame(element: 'r' | 'o', game: Game, { x, y }: Coordinate): Game {
  const newGameState = game.map((line, idx) => {
    if (idx === x) {
      const line = game[x];
      line[y] = element;

      return line;
    }

    return line;
  });

  return newGameState;
}
