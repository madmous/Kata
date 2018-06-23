import { add, concat, flow, head, map, nth, split, sum, take, without } from 'lodash/fp';

import log from './log';
import { flippedDrop, mapIndexed } from './utils';

const MAX_FRAMES = 10;
const MAX_PINS = 10;

type ScoreGame = (input: string) => number;
const scoreGame: ScoreGame = input => {
  return flow(toRolls, calculateScore([])(0))(input);
};

export { scoreGame as default };

type ToRolls = (input: string) => number[];
const toRolls: ToRolls = input => flow(split(''), fillWithNumbers)(input);

type FillWithNumbers = (input: string[]) => number[];
const fillWithNumbers: FillWithNumbers = input =>
  flow(mapIndexed(toNumberInStringIfNecessary), withoutSpaces, map(toNumber))(input);

type ToNumberInStringIfNecessary = (currentValue: string, index: number, array: string[]) => string;
const toNumberInStringIfNecessary: ToNumberInStringIfNecessary = (currentValue, index, array) => {
  switch (currentValue) {
    case '/':
      return String(10 - parseInt(nth(index - 1)(array), 10));
    case 'X':
      return '10';
    case '-':
      return '0';
    default:
      return currentValue;
  }
};

type WithoutSpaces = (input: string[]) => string[];
const withoutSpaces: WithoutSpaces = input => without([' '])(input);

type ToNumber = (input: string) => number;
const toNumber: ToNumber = input => parseInt(input, 10);

type CalculateScore = (accumulator: number[]) => (index: number) => (rolls: number[]) => number;
const calculateScore: CalculateScore = accumulator => index => rolls => {
  if (index === MAX_FRAMES) {
    return sum(accumulator);
  } else {
    const nextScores = toNextScores(accumulator)(rolls);
    const nextIndex = add(1)(index);
    const nextRolls = toNextRolls(rolls);

    return calculateScore(nextScores)(nextIndex)(nextRolls);
  }
};

type ToNextScores = (scores: number[]) => (rolls: number[]) => number[];
const toNextScores: ToNextScores = scores => rolls =>
  flow(rollsNumberToTake, calculateRollScore(rolls), concat(scores))(rolls);

type RollsNumberToTake = (rolls: number[]) => number;
const rollsNumberToTake: RollsNumberToTake = rolls => {
  if (isStrike(rolls) || isSpare(rolls)) {
    return 3;
  } else {
    return 2;
  }
};

type IsSpare = (rolls: number[]) => boolean;
const isSpare: IsSpare = rolls => sum(take(2)(rolls)) === MAX_PINS;

type CalculateRollScore = (rolls: number[]) => (nbrRolls: number) => number[];
const calculateRollScore: CalculateRollScore = rolls => nbrRolls => [sum(take(nbrRolls)(rolls))];

type ToNextRolls = (rolls: number[]) => number[];
const toNextRolls: ToNextRolls = rolls => flow(rollsNumberToDrop, flippedDrop(rolls))(rolls);

type RollsNumberToDrop = (rolls: number[]) => number;
const rollsNumberToDrop: RollsNumberToDrop = rolls => {
  if (isStrike(rolls)) {
    return 1;
  } else {
    return 2;
  }
};

type IsStrike = (rolls: number[]) => boolean;
const isStrike: IsStrike = rolls => head(rolls) === MAX_PINS;
