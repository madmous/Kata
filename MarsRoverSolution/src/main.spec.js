/** @flow */

import { execute } from './main';
import { play } from './game';

import type { Coordinate, Dimension, Obstacles, World, Rover } from './main';

const anObstacleAt33: Coordinate = {
  x: 3,
  y: 3,
};
const aRoverAt00N: Rover = {
  coordinate: {
    x: 0,
    y: 0,
  },
  direction: 'N',
};
const a55Dimension: Dimension = {
  width: 5,
  height: 5,
};

describe('Game', () => {
  xit('should create a 5 by 5 game grid when the world dimension is 5 by 5', async () => {
    // given
    const aWorld: World = {
      rover: aRoverAt00N,
      dimension: a55Dimension,
      obstacles: [anObstacleAt33],
    };

    // when
    const game = play(aWorld);

    // then
    expect(game.length).toBe(5);
  });

  xit('should have a rover at position 0,0 facing north', async () => {
    // given
    const aWorld: World = {
      rover: aRoverAt00N,
      dimension: a55Dimension,
      obstacles: [anObstacleAt33],
    };

    // when
    const game = play(aWorld);

    // then
    expect(game[0][0]).toBe('r');
  });

  xit('should have obstacles at position 3,3 and position 3,4', async () => {
    // given
    const aWorld: World = {
      rover: aRoverAt00N,
      dimension: a55Dimension,
      obstacles: [
        anObstacleAt33,
        {
          x: 3,
          y: 4,
        },
      ],
    };

    // when
    const game = play(aWorld);

    // then
    expect(game[3][3]).toBe('o');
    expect(game[3][4]).toBe('o');
  });
});

describe('Rover', () => {
  it('should move freely around the world from one edge to another when it receives commands', () => {
    // Given
    const aWorld: World = {
      rover: aRoverAt00N,
      dimension: a55Dimension,
      obstacles: [anObstacleAt33],
    };
    const commands = [
      'l',
      'f',
      'b',
      'l',
      'f',
      'b',
      'l',
      'b',
      'f',
      'r',
      'b',
      'r',
      'b',
      'f',
      'f',
    ];
    // When
    const newWorld = execute(aWorld, commands);

    // Then
    const expectedWorld: World = {
      rover: {
        coordinate: {
          x: 4,
          y: 1,
        },
        direction: 'W',
      },
      dimension: {
        width: 5,
        height: 5,
      },
      obstacles: [anObstacleAt33],
    };
    expect(newWorld).toEqual(expectedWorld);
  });

  it('should not move when an obstacle is detected', () => {
    // Given
    const aRoverAt32N: Rover = {
      ...aRoverAt00N,
      coordinate: {
        x: 3,
        y: 2,
      },
    };
    const obstacles: Obstacles = [
      anObstacleAt33,
      {
        x: 0,
        y: 2,
      },
    ];
    const aWorld: World = {
      dimension: a55Dimension,
      rover: aRoverAt32N,
      obstacles,
    };
    const commands = ['f', 'f', 'r', 'f', 'f'];
    // When
    const newWorld = execute(aWorld, commands);

    // Then
    const expectedWorld: World = {
      rover: {
        coordinate: {
          x: 4,
          y: 2,
        },
        direction: 'E',
      },
      dimension: a55Dimension,
      obstacles,
    };
    expect(newWorld).toEqual(expectedWorld);
  });
});
