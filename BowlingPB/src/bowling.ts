import { concat, drop, flow, head, last, size, split, sum, tail, take } from 'lodash/fp';

type Score = (input: string) => number[];
const score: Score = input =>
  flow(
    split(''),
    parseInput([]),
    calculateFrameScores([])
  )(input);

export default score;

type ParseInput = (parsedInput: number[]) => (input: string[]) => number[];
const parseInput: ParseInput = parsedInput => input => {
  if (size(input) === 0) {
    return parsedInput;
  } else {
    const nextParsedInput = toNextParsedInput(parsedInput)(head(input));

    return parseInput(nextParsedInput)(tail(input));
  }
};

type ToNextParsedInput = (parsedInput: number[]) => (char: string) => number[];
const toNextParsedInput: ToNextParsedInput = parsedInput => char => {
  switch (char) {
    case 'x':
      return [...parsedInput, 10];
    case '-':
      return [...parsedInput, 0];
    case '/':
      return [...parsedInput, 10 - last(parsedInput)];
    default:
      return [...parsedInput, parseInt(char, 10)];
  }
};

type CalculateFrameScores = (frameScores: number[]) => (input: number[]) => number[];
const calculateFrameScores: CalculateFrameScores = frameScores => input => {
  if (size(input) === 0) {
    return frameScores;
  } else {
    const headFrameScore = flow(
      take(2),
      sum
    )(input);

    const nextFrameScores = flow(
      framesNbrToTake,
      flippedTake(input),
      sum,
      concat(frameScores)
    )(headFrameScore);

    const nextInput = flow(
      framesNbrToDrop,
      flippedDrop(input)
    )(headFrameScore);

    return calculateFrameScores(nextFrameScores)(nextInput);
  }
};

type FramesNbr = (headFrameScore: number) => number;
const framesNbrToTake: FramesNbr = headFrameScore => {
  if (headFrameScore < 10) {
    return 2;
  } else {
    return 3;
  }
};
const framesNbrToDrop: FramesNbr = headFrameScore => {
  if (headFrameScore === 10) {
    return 1;
  } else {
    return 2;
  }
};

// UTILS

const flippedDrop = (array: number[]) => (elements: number) => drop(elements)(array);
const flippedTake = (array: number[]) => (elements: number) => take(elements)(array);
