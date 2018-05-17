import { dropRight, flow, head, indexOf, reduce, split, takeRight } from 'lodash/fp';

type Operation = (a: number, b: number) => number;

type OperationSymbol = '+' | '/' | '-' | 'x';

type OperationBySymbol = { [k: string]: Operation };

const operationBySymbol: OperationBySymbol = {
  '+': (a, b) => a + b,
  '-': (a, b) => a - b,
  '/': (a, b) => a / b,
  x: (a, b) => a * b
};

const operationSymbols: OperationSymbol[] = ['+', '/', '-', 'x'];

type CalculateOperation = (acc: number[]) => (current: string) => number;
const calculateOperation: CalculateOperation = acc => current => {
  const [first, second] = takeRight(2)(acc);

  return operationBySymbol[current](first, second);
};

type GetNextAcc =  (acc: number[]) => (current: string) => number[];
const getNextAcc: GetNextAcc = acc => current => {
  if (isOperationSymbol(current)) {
    return dropRight(2)(acc);
  } else {
    return acc;
  }
};

type GetNextValue =  (acc: number[]) => (current: string) => number;
const getNextValue: GetNextValue = acc => current => {
  if (isOperationSymbol(current)) {
    return calculateOperation(acc)(current);
  } else {
    return parseInt(current, 10);
  }
};

type IsOperationSymbol = (value: string) => boolean;
const isOperationSymbol: IsOperationSymbol = value => indexOf(value)(operationSymbols) !== -1;

type CalculateOperations = (acc: number[], current: string) => number[];
const calculateOperations: CalculateOperations = (acc, current) => {
  const nextAcc = getNextAcc(acc)(current);
  const nextValue = getNextValue(acc)(current);

  return [...nextAcc, nextValue];
};

type Evaluate = (input: string) => number;
const evaluate: Evaluate = input => flow(split(' '), reduce(calculateOperations)([]), head)(input);

export { evaluate as default };
