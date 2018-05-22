import {
  concat,
  drop,
  head,
  join,
  flow,
  map,
  nth,
  reduce,
  tail,
} from 'lodash/fp';

import log from './log';




type GetNextCurrentCells = (currentCell: string) => (rightCell: string) => (cells: string[]) => string[];
const getNextCurrentCells: GetNextCurrentCells = currentCell => rightCell => cells => {
  if (currentCell === '*') {
    if (rightCell !== undefined && rightCell !== '*' && rightCell !== ' ') {
      return drop(2)(cells);
    } else {
      return drop(1)(cells);
    }
  } else {
    return drop(1)(cells);
  }
}

type GetNextAcc = (currentCell: string) => (rightCell: string) => (acc: string[]) => string[];
const getNextAcc: GetNextAcc = currentCell => rightCell => acc => {
  if (currentCell === '*') {
    if (rightCell !== undefined && rightCell !== '*' && rightCell !== ' ') {
      return [...acc, '*', '1'];
    } else {
      return [...acc, currentCell];
    }
  } else {
    return [...acc, currentCell];
  }
}

type Count = (acc: string[]) => (currentCells: string[]) => string[];
const count: Count = acc => currentCells => {
  if (currentCells.length === 0) {
    return acc;
  } else {
    const cell = head(currentCells);
    const rightCell = nth(1)(currentCells);

    const nextAcc = getNextAcc(cell)(rightCell)(acc);
    const nextCurrentCells = getNextCurrentCells(cell)(rightCell)(currentCells);

    return count(nextAcc)(nextCurrentCells);
  }
};

type ToCell = (inputCell: string) => string;
const toCell: ToCell = inputCell => {
  if (inputCell === '.') {
    return '0';
  } else {
    return inputCell;
  }
};

type FindMines = (input: string) => string;
const findMines: FindMines = input => {
  return flow(map(toCell), count([]), join(''))(input);
};

export { findMines as default };
