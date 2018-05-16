import evaluate from './main';

describe('RPN Calculator', () => {
  [
    { input: '5 3 +', expectedResult: 8 },
    { input: '6 2 /', expectedResult: 3 },
    { input: '5 2 - 7 +', expectedResult: 10 },
    { input: '3 4 2 1 + x + 2 /', expectedResult: 7.5 },
    { input: '3 5 8 x 7 + x', expectedResult: 141 },
    { input: '1 2 + 4 x 5 + 3 -', expectedResult: 14 },
    { input: '5 4 1 2 + x +', expectedResult: 17 },
  ].forEach(({ input, expectedResult }) => {
    it(`should correctly evaluate ${input}`, () => {
      // given

      // when
      const result = evaluate(input);

      // then
      expect(result).toEqual(expectedResult);
    });
  });

  xit('should handle the square root operation', () => {
      // given
      const input = '9 SQRT';

      // when
      const result = evaluate(input);

      // then
      expect(result).toEqual(3);
  });

  [
    { input: '5 3 4 2 9 1 MAX', expectedResult: 9 },
    // { input: '4 5 MAX 1 2 MAX *', expectedResult: 10 },
  ].forEach(({ input, expectedResult }) => {
    xit('should handle the max operation', () => {
      // given

      // when
      const result = evaluate(input);

      // then
      expect(result).toEqual(expectedResult);
  });
  });
});
