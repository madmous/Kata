/** @flow */

import {
  updateGameScore,
  decideGameWinner,
  printScore,
} from './main';

describe('Player 1', () => {
  it('should win the game when the score is 4-0', () => {
    // given
    const player1Points = 4;
    const player2Points = 0;

    // when
    const game = updateGameScore(player1Points, player2Points);
    const winner = decideGameWinner(game);

    // then
    expect(winner).toEqual('Player 1');
  });

  it('should win the game when the score is 5-3', () => {
    // given
    const player1Points = 5;
    const player2Points = 3;

    // when
    const game = updateGameScore(player1Points, player2Points);
    const winner = decideGameWinner(game);

    // then
    expect(winner).toEqual('Player 1');
  });
});

describe('Player 2', () => {
  it('should win the game when the score is 4-0', () => {
    // given
    const player1Points = 0;
    const player2Points = 4;

    // when
    const game = updateGameScore(player1Points, player2Points);
    const winner = decideGameWinner(game);

    // then
    expect(winner).toEqual('Player 2');
  });

  it('should win the game when the score is 3-5', () => {
    // given
    const player1Points = 3;
    const player2Points = 5;

    // when
    const game = updateGameScore(player1Points, player2Points);
    const winner = decideGameWinner(game);

    // then
    expect(winner).toEqual('Player 2');
  });
});

describe('Game', () => {
  it('should print thirty - thirty when the score is 2-2', () => {
    // given
    const player1Points = 2;
    const player2Points = 2;

    // when
    const score = printScore([player1Points, player2Points]);

    // then
    expect(score).toEqual('thirty - thirty');
  });

  it('should print deuce when the score is 3-3', () => {
    // given
    const player1Points = 3;
    const player2Points = 3;

    // when
    const score = printScore([player1Points, player2Points]);

    // then
    expect(score).toEqual('deuce');
  });

  it('should print advantage player 1 when the score is 4-3', () => {
    // given
    const player1Points = 4;
    const player2Points = 3;

    // when
    const score = printScore([player1Points, player2Points]);

    // then
    expect(score).toEqual('advantage player 1');
  });

  it('should print advantage player 2 when the score is 5-6', () => {
    // given
    const player1Points = 5;
    const player2Points = 6;

    // when
    const score = printScore([player1Points, player2Points]);

    // then
    expect(score).toEqual('advantage player 2');
  });
});
