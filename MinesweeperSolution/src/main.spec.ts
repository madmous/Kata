import findMines from './main';

describe('Find mines', () => {
  [
    { field: '.', result: '0' },
    { field: '... ...', result: '000 000' },
  ].forEach(({ field, result }) => {
    it(`should not find any adjacent cell to a mine when the field has no mines ${field}`, () => {
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
    it(`should not find adjacent cell to a mine when the field is full of mines ${field}`, () => {
    // when
    const mines = findMines(field);

    // then
    expect(mines).toEqual(result);
    });
  });

  [
    { field: '*.', result: '*1' },
    { field: '*... ....', result: '*100 1100' },
    { field: '*... .... .*.. ....', result: '*100 2210 1*10 1110' },
    { field: '**... ..... .*...', result: '**100 33200 1*100' },
  ].forEach(({ field, result }) => {
    it(`should correctly count adjacent cells to a mine when the field has mines ${field}`, () => {
    // when
    const mines = findMines(field);

    // then
    expect(mines).toEqual(result);
    });
  });
});
