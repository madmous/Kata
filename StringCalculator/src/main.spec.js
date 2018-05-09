/** @flow */

import addNumbers from './main';

describe('Add numbers', () => {
  xit('should return 0 when an empty string is passed', () => {
    //given
    const numbers = '';

    //when
    const sum = addNumbers(numbers);

    //then
    expect(sum).toEqual(0);
  });

  xit('should handle an unknown amount of numbers', () => {
    //given
    const numbers = '10,2,3,2,1';

    //when
    const sum = addNumbers(numbers);

    //then
    expect(sum).toEqual(18);
  });

  xit('should handle handle new lines between numbers', () => {
    //given
    const numbers = '1\n2,10';

    //when
    const sum = addNumbers(numbers);

    //then
    expect(sum).toEqual(13);
  });

  xit('should handle delimeter changes', () => {
    //given
    const numbers = '//8\n182';

    //when
    const sum = addNumbers(numbers);

    //then
    expect(sum).toEqual(3);
  });

  xit('should throw an exception when negative numbers are present', () => {
    //given
    const numbers = '1,-2';

    //then
    expect(() => {
      addNumbers(numbers);
    }).toThrow('negatives not allowed');
  });

  xit('should filter numbers bigger than 1000 when adding', () => {
    //given
    const numbers = '1,1002';

    //when
    const sum = addNumbers(numbers);

    //then
    expect(sum).toEqual(1);
  });

  xit('should handle delimeter of any length', () => {
    //given
    const numbers = '//[***]\n1***20***3';

    //when
    const sum = addNumbers(numbers);

    //then
    expect(sum).toEqual(24);
  });

  xit('should handle multiple delimxiters', () => {
    //given
    const numbers = '//[**][6]\n1**263';

    //when
    const sum = addNumbers(numbers);

    //then
    expect(sum).toEqual(6);
  });
});
