/** @flow */

import scoreRolls from './main';

describe('Add rolls', () => {
  xit('should return 8 when player fails to knock down all pins in two tries', () => {
    //given
    const numbers = '53';

    //when
    const sum = scoreRolls(numbers);

    //then
    expect(sum).toEqual(8);
  });
});
