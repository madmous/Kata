/** @flow */

import findMines from './main';

describe('Find mines', () => {
  it('should not find any mine when the field has no mines', () => {
    //given
    const field = '*... .... .*.. ....';

    //when
    const mines = findMines(field);

    //then
    expect(mines).toEqual('*100 2210 1*10 1110');
  });
});
