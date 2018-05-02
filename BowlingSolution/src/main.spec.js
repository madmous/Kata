/** @flow */

import calculateScore from './main';

describe('Add rolls', () => {
  it('should return add the score of the 2 tries when the player fails to knock down all pins', () => {
    //given
    const numbers = '53';

    //when
    const sum = calculateScore(numbers);

    //then
    expect(sum).toEqual(8);
  });

  xit('should score ten plus the number of pins knocked down on the next throw when the player scores a spare', () => {
    //given
    const numbers = '6/ 81';

    //when
    const sum = calculateScore(numbers);

    //then
    expect(sum).toEqual(27);
  });

  xit('should score ten plus the number of pins knocked down on the thwo next throws when the player scores a spare', () => {
    //given
    const numbers = 'X 81';

    //when
    const sum = calculateScore(numbers);

    //then
    expect(sum).toEqual(28);
  });

  xit('should let the player throw one more ball when it is his last throw', () => {
    //given
    const numbers = 'XXX';

    //when
    const sum = calculateScore(numbers);

    //then
    expect(sum).toEqual(30);
  });

  xit('should let the player throw one more ball when it is his last throw', () => {
    //given
    const numbers = '9/X';

    //when
    const sum = calculateScore(numbers);

    //then
    expect(sum).toEqual(20);
  });

  xit('should let the player throw one more ball when it is his last throw', () => {
    //given
    const numbers = 'X6/';

    //when
    const sum = calculateScore(numbers);

    //then
    expect(sum).toEqual(20);
  });
});
