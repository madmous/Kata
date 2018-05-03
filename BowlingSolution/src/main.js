/** @flow */

import {
  concat,
  head,
  inc,
  map,
  nth,
  pipe,
  split,
  sum,
  take,
  without,
} from 'ramda';

import { flippedDrop, mapIndexed } from './utils';

// import log from './log';

const MAX_FRAMES = 10;
const MAX_PINS = 10;

type CalculateRollScore = (rolls: number[]) => (nbrRolls: number) => number[];
const calculateRollScore: CalculateRollScore = rolls => nbrRolls => [
  sum(take(nbrRolls)(rolls)),
];

type IsStrike = (rolls: number[]) => boolean;
const isStrike: IsStrike = rolls => head(rolls) === MAX_PINS;

type IsSpare = (rolls: number[]) => boolean;
const isSpare: IsSpare = rolls => sum(take(2)(rolls)) === MAX_PINS;

type RollsNumberToDrop = (rolls: number[]) => number;
const rollsNumberToDrop: RollsNumberToDrop = rolls => {
  if (isStrike(rolls)) {
    return 1;
  } else {
    return 2;
  }
};

type RollsNumberToTake = (rolls: number[]) => number;
const rollsNumberToTake: RollsNumberToTake = rolls => {
  if (isStrike(rolls) || isSpare(rolls)) {
    return 3;
  } else {
    return 2;
  }
};

type ToNextRolls = (rolls: number[]) => number[];
const toNextRolls: ToNextRolls = rolls =>
  pipe(rollsNumberToDrop, flippedDrop(rolls))(rolls);

type ToNextScores = (scores: number[]) => (rolls: number[]) => number[];
const toNextScores: ToNextScores = scores => rolls =>
  pipe(rollsNumberToTake, calculateRollScore(rolls), concat(scores))(rolls);

type CalculateScore = (
  scores: number[]
) => (index: number) => (rolls: number[]) => number[];
const calculateScore: CalculateScore = scores => index => rolls => {
  if (index === MAX_FRAMES) {
    return sum(scores);
  } else {
    const nextScores = toNextScores(scores)(rolls);
    const nextIndex = inc(index);
    const nextRolls = toNextRolls(rolls);

    return calculateScore(nextScores)(nextIndex)(nextRolls);
  }
};

type ToNumbers = (input: string[]) => number[];
const toNumbers: ToNumbers = input => map(parseInt)(input);

type WithoutSpaces = (input: string[]) => number[];
const withoutSpaces: WithoutSpaces = input => without(' ')(input);

type ToNumbersInStringIfNecessary = (
  currentValue: string,
  index: number,
  array: string[]
) => string;
const toNumbersInStringIfNecessary: ToNumbersInStringIfNecessary = (
  currentValue,
  index,
  array
) => {
  if (currentValue === '/') {
    return String(10 - parseInt(nth(index - 1)(array)));
  } else if (currentValue === 'X') {
    return '10';
  } else if (currentValue === '-') {
    return '0';
  } else {
    return currentValue;
  }
};

type ToNumbersInString = (input: string[]) => string[];
const toNumbersInString: ToNumbersInString = input =>
  mapIndexed(toNumbersInStringIfNecessary)(input);

type FillWithNumbers = (input: string[]) => number[];
const fillWithNumbers: FillWithNumbers = input =>
  pipe(toNumbersInString, withoutSpaces, toNumbers)(input);

type ToRolls = (input: string) => number[];
const toRolls: ToRolls = input => pipe(split(''), fillWithNumbers)(input);

type ScoreGame = (input: string) => number;
const scoreGame: ScoreGame = input =>
  pipe(toRolls, calculateScore([])(0))(input);

export { scoreGame as default };
