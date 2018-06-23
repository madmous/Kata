import evaluate from './main';

describe('RPN Calculator', () => {
  [
    { input: '', expectedResult: 0 },
    { input: '5 3 +', expectedResult: 8 },
    { input: '6 2 /', expectedResult: 3 },
    { input: '5 2 - 7 +', expectedResult: 10 },
    { input: '3 4 2 1 + * + 2 /', expectedResult: 7.5 },
    { input: '3 5 8 * 7 + *', expectedResult: 141 },
    { input: '1 2 + 4 * 5 + 3 -', expectedResult: 14 },
    { input: '5 4 1 2 + * +', expectedResult: 17 },
  ].forEach(({ input, expectedResult }) => {
    it(`should correctly evaluate ${input}`, () => {
      expect(evaluate(input)).toEqual(expectedResult);
    });
  });

  xit('should handle the square root operation', () => {
      expect(evaluate('9 SQRT')).toEqual(3);
  });

  [
    { input: '5 3 4 2 9 1 MAX', expectedResult: 9 },
    // { input: '4 5 MAX 1 2 MAX *', expectedResult: 10 },
  ].forEach(({ input, expectedResult }) => {
    xit('should handle the max operation', () => {
      expect(evaluate(input)).toEqual(expectedResult);
  });
  });
});
