/** @flow */

import scoreGame from './main';

describe('Add rolls', () => {
  [
    { rolls: '--', result: 0 },
    { rolls: '1', result: 1 },
    { rolls: '13', result: 4 },
    { rolls: '13521', result: 12 },
  ].forEach(({ rolls, result }) => {
    it(`should calculate scores when there are no strikes or spares: ${rolls}`, () => {
      //given

      //when
      const score = scoreGame(rolls);

      //then
      expect(score).toEqual(result);
    });
  });

  [
    { rolls: '1-5-', result: 6 },
    { rolls: '9-9-9-9-9-9-9-9-9-9-', result: 90 },
  ].forEach(({ rolls, result }) => {
    it(`should calculate scores when there is a miss: ${rolls}`, () => {
      //given

      //when
      const score = scoreGame(rolls);

      //then
      expect(score).toEqual(result);
    });
  });

  [
    { rolls: '1/', result: 10 },
    { rolls: '1/--', result: 10 },
    { rolls: '1/-5', result: 15 },
    { rolls: '1/35-', result: 21 },
    { rolls: '1/3/23', result: 30 },
    { rolls: '5/5/5/5/5/5/5/5/5/5/5', result: 150 },
  ].forEach(({ rolls, result }) => {
    it(`should calculate scores when there are spares: ${rolls}`, () => {
      //given

      //when
      const score = scoreGame(rolls);

      //then
      expect(score).toEqual(result);
    });
  });

  [
    { rolls: 'X', result: 10 },
    { rolls: 'X--', result: 10 },
    { rolls: 'X--51', result: 16 },
    { rolls: 'X51', result: 22 },
    { rolls: 'XXXXXXXXXXXX', result: 300 },
    { rolls: 'XXXXXXXXXX12', result: 274 },
    { rolls: '1/35XXX45', result: 103 },
    { rolls: '1/35XXX458/X35', result: 149 },
    { rolls: '1/35XXX458/X3/', result: 153 },
    { rolls: '1/35XXX458/X3/23', result: 160 },
    { rolls: '1/35XXX458/X3/X', result: 173 },
    { rolls: '1/35XXX458/X3/XX6', result: 189 },
  ].forEach(({ rolls, result }) => {
    it(`should calculate scores when there are strikes: ${rolls}`, () => {
      //given

      //when
      const score = scoreGame(rolls);

      //then
      expect(score).toEqual(result);
    });
  });
});
