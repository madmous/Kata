import { flow, flowRight, head, reduce, split } from 'lodash/fp';

type GenerateOutput = (input: string) => number;
const generateOutput: GenerateOutput = input => {
  if (input === '') {
    return 0;
  } else {
    return flow(split(' '), calculate, head)(input);
  }
};
export default generateOutput;

type Calculate = (inputs: string[]) => number[];
const calculate: Calculate = inputs => {
  const reduceFunction = (acc: number[], current: string) => {
    const [x, y, ...rest] = acc;

    switch (current) {
      case '+': return [y + x, ...rest];
      case '/': return [y / x, ...rest];
      case '-': return [y - x, ...rest];
      case '*': return [y * x, ...rest];
      default: return [parseInt(current, 10), ...acc];
    }
  };

  return reduce(reduceFunction)([])(inputs);
};
