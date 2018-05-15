import calculate, { extractDatas } from './main';

describe('RPN Calculator', () => {
  it('should add a pair', () => {
    // given
    const input = '5 3 +';

    // when
    const result = calculate(input);

    // then
    expect(result).toEqual(8);
  });

  it('should divide a pair', () => {
    // given
    const input = '6 2 /';

    // when
    const result = calculate(input);

    // then
    expect(result).toEqual(3);
  });

  it('should calculate when operand after values', () => {
    // given
    const input = '5 2 - 7 +';

    // when
    const result = calculate(input);

    // then
    expect(result).toEqual(10);
  });

  it('should calculate when operand after values', () => {
    // given
    const input = '3 4 2 1 + x + 2 /';

    // when
    const result = calculate(input);

    // then
    expect(result).toEqual(7.5);
  });

  it('should calculate when operand after values', () => {
    // given
    const input = '3 5 8 x 7 + x';

    // when
    const result = calculate(input);

    // then
    expect(result).toEqual(141);
  });

  it('should calculate when operand after values', () => {
    // given
    const input = '1 2 + 4 x 5 + 3 -';

    // when
    const result = calculate(input);

    // then
    expect(result).toEqual(14);
  });

  it('should calculate when operand after values', () => {
    // given
    const input = '5 4 1 2 + x +';

    // when
    const result = calculate(input);

    // then
    expect(result).toEqual(17);
  });
});
