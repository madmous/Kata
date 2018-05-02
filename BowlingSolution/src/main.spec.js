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

  it('should score ten plus the number of pins knocked down on the next throw when the player scores a spare', () => {
    //given
    const numbers = '6/ 81';

    //when
    const sum = calculateScore(numbers);

    //then
    expect(sum).toEqual(27);
  });

  it('should score ten plus the number of pins knocked down on the thwo next throws when the player scores a spare', () => {
    //given
    const numbers = 'X 81';

    //when
    const sum = calculateScore(numbers);

    //then
    expect(sum).toEqual(28);
  });

  it('should let the player throw one more ball when it is his last throw', () => {
    //given
    const numbers = 'XXX';

    //when
    const sum = calculateScore(numbers);

    //then
    expect(sum).toEqual(30);
  });

  it('should let the player throw one more ball when it is his last throw', () => {
    //given
    const numbers = '9/X';

    //when
    const sum = calculateScore(numbers);

    //then
    expect(sum).toEqual(20);
  });

  it('should let the player throw one more ball when it is his last throw', () => {
    //given
    const numbers = 'X6/';

    //when
    const sum = calculateScore(numbers);

    //then
    expect(sum).toEqual(20);
  });

  it('should calculate the game score', () => {
    //given
    const numbers = '9- 9- 9- 9- 9- 9- 9- 9- 9- 9-';

    //when
    const sum = calculateScore(numbers);

    //then
    expect(sum).toEqual(90);
  });

  it('should calculate the game score', () => {
    //given
    const numbers = '5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/ 5/5';

    //when
    const sum = calculateScore(numbers);

    //then
    expect(sum).toEqual(150);
  });

  it.only('should calculate the game score', () => {
    //given
    const numbers = 'X X X';

    //when
    const sum = calculateScore(numbers);

    //then
    expect(sum).toEqual(60);
  });
});
