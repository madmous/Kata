/** @flow */

type Point = number;
type GameScore = number[];
type Winner = 'Player 1' | 'Player 2' | 'Game still on';
type Score =
  | 'fifteen'
  | 'thirty'
  | 'forty'
  | 'love'
  | 'deuce'
  | 'advantage player 1'
  | 'advantage player 2';

// type IncrementPlayerPoints = (point: Point) => Point;

type UpdateGame = (player1Points: Point, player2Points: Point) => GameScore;

type DecideGameWinner = (game: GameScore) => Winner;

type PrintScore = (gameScore: GameScore) => string;

type ToGameScoreString = (game: GameScore) => Score[];

const toGameScoreString: ToGameScoreString = game => {
  const TWO_POINTS = 2;
  const [player1Points, player2Points] = game;
  const scores = {
    '0': 'love',
    '1': 'fifteen',
    '2': 'thirty',
    '3': 'forty',
  };
  const arePointsAboveTwo = player1Points > TWO_POINTS && player2Points > TWO_POINTS;
  const isPlayer1PointsAbovePlayer2Points = player1Points > player2Points;
  const arePointsEqual = player1Points === player2Points;
  const playerAdvantage = isPlayer1PointsAbovePlayer2Points
    ? 'advantage player 1'
    : 'advantage player 2';
  const deuceOrAdvantage = arePointsEqual ? 'deuce' : playerAdvantage;

  return arePointsAboveTwo
    ? [deuceOrAdvantage]
    : [scores[player1Points], scores[player2Points]];
};

export const printScore: PrintScore = game => toGameScoreString(game).join(' - ');

// const incrementPlayerPoints: IncrementPlayerPoints = point => ++point;

export const updateGameScore: UpdateGame = (player1Points, player2Points) => [
  player1Points,
  player2Points,
];

export const decideGameWinner: DecideGameWinner = game => {
  const [player1Points, player2Points] = game;

  const isWinnerPlayer1 =
    player1Points === 4 || player1Points - player2Points === 2;
  const isWinnerPlayer2 =
    player2Points === 4 || player2Points - player1Points === 2;

  if (isWinnerPlayer1) {
    return 'Player 1';
  } else if (isWinnerPlayer2) {
    return 'Player 2';
  } else {
    return 'Game still on';
  }
};