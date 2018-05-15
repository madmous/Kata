import { dropRight, flow, head, reduce, split, takeRight } from 'lodash/fp';

type Operation = (a: number, b: number) => number;

const sum: Operation = (a, b) => a + b;

const difference: Operation = (a, b) => a - b;

const multiply: Operation = (a, b) => a * b;

const divide: Operation = (a, b) => a / b;

type OperationBySymbol = { [k: string]: Operation };

const operationBySymbol: OperationBySymbol = {
  '+': sum,
  '-': difference,
  '/': divide,
  'x': multiply,
};

type Execute = (acc: number[], current: string) => number[];
const execute: Execute = (acc, current) => {
  const value = parseInt(current, 10);

  if (isNaN(value)) {
    const operation = operationBySymbol[current];
    const [first, second] = takeRight(2)(acc);
    const sumLast2 = operation(first, second);
    return [...dropRight(2)(acc), sumLast2];
  } else {
    return [...acc, value];
  }
};

type Calculate = (input: string) => number;
const calculate: Calculate = input => flow(split(' '), reduce(execute)([]), head)(input);

export { calculate as default };
