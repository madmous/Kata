import findMines from './main';

describe('Find mines', () => {
  [
    { field: '.', result: '0' },
    { field: '... ...', result: '000 000' },
  ].forEach(({ field, result }) => {
    it(`should not find any adjacent cell to a mine when the field has no mines`, () => {
    // when
    const mines = findMines(field);

    // then
    expect(mines).toEqual(result);
    });
  });

  [
    { field: '*', result: '*' },
    { field: '*** ***', result: '*** ***' },
  ].forEach(({ field, result }) => {
    it(`should not find adjacent cell to a mine when the field is full of mines`, () => {
    // when
    const mines = findMines(field);

    // then
    expect(mines).toEqual(result);
    });
  });

  [
    { field: '*.', result: '*1' },
  ].forEach(({ field, result }) => {
    it(`should find adjacent cell to a mine when the field has mines`, () => {
    // when
    const mines = findMines(field);

    // then
    expect(mines).toEqual(result);
    });
  });

  xit('should not find any mine when the field has no mines', () => {
    // given
    const field = `*... .... .*.. ....`;

    // when
    const mines = findMines(field);

    // then
    expect(mines).toEqual('*100 2210 1*10 1110');
  });

  xit('should not find any mine when the field has no mines', () => {
    // given
    const field = '**... ..... .*...';

    // when
    const mines = findMines(field);

    // then
    expect(mines).toEqual('**100 33200 1*100');
  });
});
