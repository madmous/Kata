import {
  concat,
  drop,
  flow,
  head,
  join,
  map,
  nth,
  reduce,
  tail,
} from 'lodash/fp';

import log from './log';

type FindMines = (input: string) => string;
const findMines: FindMines = input => {
  return flow(parse, count([]), generateOutput)(input);
};

export { findMines as default };

type Parse = (input: string) => string[];
const parse: Parse = input => map(toCell)(input);

type ToCell = (anInput: string) => string;
const toCell: ToCell = anInput => {
  if (anInput === '.') {
    return '0';
  } else {
    return anInput;
  }
};

type Count = (cellsWithMineCount: string[]) => (currentCells: string[]) => string[];
const count: Count = cellsWithMineCount => currentCells => {
  if (currentCells.length === 0) {
    return cellsWithMineCount;
  } else {
    const cell = head(currentCells);
    const rightCell = nth(1)(currentCells);

    const nextAcc = getNextAcc(cell)(rightCell)(cellsWithMineCount);
    const nextCurrentCells = getNextCurrentCells(cell)(rightCell)(currentCells);

    return count(nextAcc)(nextCurrentCells);
  }
};

type GetNextAcc = (currentCell: string) => (rightCell: string) => (cellsWithMineCount: string[]) => string[];
const getNextAcc: GetNextAcc = currentCell => rightCell => cellsWithMineCount => {
  if (currentCell === '*') {
    if (rightCell !== undefined && rightCell !== '*' && rightCell !== ' ') {
      return [...cellsWithMineCount, '*', '1'];
    } else {
      return [...cellsWithMineCount, currentCell];
    }
  } else {
    return [...cellsWithMineCount, currentCell];
  }
};

type GetNextCurrentCells = (currentCell: string) => (rightCell: string) => (cells: string[]) => string[];
const getNextCurrentCells: GetNextCurrentCells = currentCell => rightCell => cells => {
  console.log('currentCell', currentCell);
  console.log('rightCell', rightCell);
  console.log('cells', cells);
  if (currentCell === '*') {
    if (rightCell !== undefined && rightCell === '0') {
      return drop(2)(cells);
    } else {
      return drop(1)(cells);
    }
  } else {
    return drop(1)(cells);
  }
};

type GenerateOutput = (cells: string[]) => string;
const generateOutput: GenerateOutput = cells => join('')(cells);
