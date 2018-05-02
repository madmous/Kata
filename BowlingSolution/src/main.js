/** @flow */

import { head, last, map, pipe, reduce, split, sum, tail } from 'ramda';

import log from './log';

type Score = number[];
type Status = 'STRIKE' | 'SPARE' | 'NOTHING';

type Frame = {
  status: Status,
  score: Score,
};

type CreateFrameScore = (acc: Frame, frame: Frame) => Frame;
const createFrameScore: CreateFrameScore = (acc, frame) => {
  if (acc.status === 'SPARE') {
    return {
      status: frame.status,
      score: [sum(acc.score) + head(frame.score) + sum(frame.score)],
    };
  } else if (acc.status === 'STRIKE') {
    console.log('fdsf', tail(frame.score))
    return {
      status: frame.status,
      score: [
        sum(acc.score) +
          head(frame.score) +
          last(frame.score) +
          sum(frame.score),
      ],
    };
  } else {
    return {
      status: frame.status,
      score: [sum(acc.score) + sum(frame.score)],
    };
  }
};

type ToFrameScore = (frames: Frame[]) => number;
const toFrameScore: ToFrameScore = frames => {
  const { score } = head(frames);

  if (frames.length === 1) {
    return sum(score);
  } else {
    console.log('frames', frames)
    const frame = reduce(createFrameScore)(head(frames))(tail(frames));
    const score = sum(frame.score);
    return score;
  }
};

type CreateScore = (acc: number[], character: string) => number[];
const createScore: CreateScore = (acc, character) => {
  if (character === '/') {
    return [...acc, 10 - parseInt(last(acc))];
  } else if (character === 'X') {
    return [...acc, 10];
  } else if (character === '-') {
    return [...acc, 0];
  } else {
    return [...acc, parseInt(character)];
  }
};

type ToScore = (characters: string) => Score;
const toScore: ToScore = characters => reduce(createScore)([])(characters);

type ToStatus = (character: string) => Status;
const toStatus: ToStatus = character => {
  const firstTry = head(character);
  const secondTry = last(character);

  if (firstTry === 'X') {
    return 'STRIKE';
  } else if (secondTry === '/') {
    return 'SPARE';
  } else {
    return 'NOTHING';
  }
};

type ToFrame = (characters: string) => Frame;
const toFrame: ToFrame = characters => {
  const status = toStatus(characters);
  const score = toScore(characters);

  return {
    status,
    score,
  };
};

type ToFrames = (characters: string[]) => Frame[];
const toFrames: ToFrames = characters => map(toFrame)(characters);

type ToArray = (characters: string) => string[];
const toArray: ToArray = characters => split(' ')(characters);

type CalculateScore = (input: string) => number;
const calculateScore: CalculateScore = input => {
  return pipe(toArray, toFrames, toFrameScore)(input);
};

export { calculateScore as default };
