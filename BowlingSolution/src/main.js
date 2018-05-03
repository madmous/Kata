/** @flow */

import {
  addIndex,
  concat,
  drop,
  filter,
  head,
  map,
  nth,
  pipe,
  split,
  sum,
  take,
} from 'ramda';

import log from './log';

type CalculateRollsScore = (nbrRolls: number) => (rolls: number[]) => boolean;
const calculateRollsScore: CalculateRollsScore = nbrRolls => rolls =>
  sum(take(nbrRolls)(rolls));

type IsStrike = (rolls: number[]) => boolean;
const isStrike: IsStrike = rolls => head(rolls) === 10;

type IsSpare = (rolls: number[]) => boolean;
const isSpare: IsSpare = rolls => sum(take(2)(rolls)) === 10;

const g = rolls => {
  if (isStrike(rolls)) {
    return {
      take: 3,
      dropNbr: 1,
    };
  } else if (isSpare(rolls)) {
    return {
      take: 3,
      dropNbr: 2,
    };
  } else {
    return {
      take: 2,
      dropNbr: 2,
    };
  }
};

type CalculateScore = (scores: number[]) => (rolls: number[]) => number[];
const calculateScore: CalculateScore = scores => rolls => {
  if (rolls.length === 0) {
    return sum(scores);
  } else {
    if (rolls.length === 3) {
      const bonus = calculateRollsScore(3)(rolls);
      return calculateScore(concat(scores)([bonus]))(drop(3)(rolls));
    } else {
      const { take, dropNbr } = g(rolls);
      const bonus = calculateRollsScore(take)(rolls);
      return calculateScore(concat(scores)([bonus]))(drop(dropNbr)(rolls));
    }
  }
};

type ToNumbers = (input: string[]) => number[];
const toNumbers: ToNumbers = input => map(parseInt)(input);

type RemoveSpaces = (input: string[]) => number[];
const removeSpaces: RemoveSpaces = input => filter(v => v !== ' ')(input);

type ReplaceSpecialCharacters = (input: string[]) => string[];
const replaceSpecialCharacters: ReplaceSpecialCharacters = input => {
  const mapIndexed = addIndex(map());

  return mapIndexed((currentValue, index, array) => {
    if (currentValue === '/') {
      return String(10 - parseInt(nth(index - 1)(array)));
    } else if (currentValue === 'X') {
      return '10';
    } else if (currentValue === '-') {
      return '0';
    } else {
      return currentValue;
    }
  })(input);
};

type FillWithNumbers = (input: string[]) => number[];
const fillWithNumbers: FillWithNumbers = input => {
  return pipe(replaceSpecialCharacters, removeSpaces, toNumbers)(input);
};

type ToRolls = (input: string) => number[];
const toRolls: ToRolls = input => {
  return pipe(split(''), fillWithNumbers)(input);
};

type ScoreGame = (input: string) => number;
const scoreGame: ScoreGame = input => {
  return pipe(toRolls, calculateScore([]))(input);
};

export { scoreGame as default };
