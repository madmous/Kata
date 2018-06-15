import { bless, check, forall, generator, random, shrink } from 'jsverify';

import { isEqual } from 'lodash';
import { head, join, map, size } from 'lodash/fp';

import score from './bowling';

describe('BowlingProperty', () => {
  const checkOptions = {
    quiet: true,
    tests: 10
  };

  it('Frame score is empty when game has not started', () => {
    const prop = check(forall(arbEmptyRolls(), frames => size(score(frames)) === 0), checkOptions);

    expect(prop).toBe(true);
  });

  it('Frame score is 0 when player only misses', () => {
    const prop = check(
      forall(arbMissingRolls(), ({ frames, framesNbr }) => {
        const scores = score(frames);

        return size(scores) === framesNbr && scores.every((score: number) => score === 0);
      }),
      checkOptions
    );

    expect(prop).toBe(true);
  });

  it('Frame score is 10 plus the number of pins knocked down on the next throw when player spares', () => {
    const prop = check(
      forall(
        spareFollowedByArbRolls(),
        ({ frames, secondRoll }) => head(score(frames)) === 10 + secondRoll
      ),
      checkOptions
    );

    expect(prop).toBe(true);
  });

  it('Frame score is 10 plus the number of pins knocked down on the next two throws when player strikes', () => {
    const prop = check(
      forall(
        strikeFollowedByArbRolls(),
        ({ frames, firstRoll, secondRoll }) => head(score(frames)) === 10 + firstRoll + secondRoll
      ),
      checkOptions
    );

    expect(prop).toBe(true);
  });

  xit('Frame score on the tenth frame is the sum of all rolls', () => {
    const prop = check(
      forall(strikeRollsOnTenthFrame(), ({ frames }) =>
        score(frames).every((score: number) => score === 10)
      ),
      checkOptions
    );

    expect(prop).toBe(true);
  });
});

const arbEmptyRolls = () =>
  bless({
    generator: generator.bless(() => ''),
    show: val => val,
    shrink: shrink.noop
  });

const arbMissingRolls = () =>
  bless({
    generator: generator.bless(() => {
      const framesNbr = random(1, 10);
      const MISSES = '--';

      return {
        frames: join('')(new Array(framesNbr).fill(MISSES)),
        framesNbr
      };
    }),
    show: val => val,
    shrink: shrink.noop
  });

const spareFollowedByArbRolls = () =>
  bless({
    generator: generator.bless(() => {
      const secondRoll = random(0, 9);
      const SPARE = '/';
      const firstRoll = random(0, 9);

      return {
        frames: `${firstRoll}${SPARE}${secondRoll}-`,
        secondRoll
      };
    }),
    show: val => val,
    shrink: shrink.noop
  });

const strikeFollowedByArbRolls = () =>
  bless({
    generator: generator.bless(() => {
      const firstRoll = random(0, 9);
      const secondRoll = random(0, 9);
      const STRIKE = 'x';

      return {
        firstRoll,
        frames: `${STRIKE}${firstRoll}${secondRoll}`,
        secondRoll
      };
    }),
    show: val => val,
    shrink: shrink.noop
  });

const strikeRollsOnTenthFrame = () =>
  bless({
    generator: generator.bless(() => {
      const STRIKE = 'x';
      const firstRoll = STRIKE;
      const secondRoll = STRIKE;
      const potentialThirdRoll = STRIKE;

      return {
        // firstRoll,
        frames: `${firstRoll}${secondRoll}${potentialThirdRoll}`
        // potentialThirdRoll,
        // secondRoll,
      };
    }),
    show: val => val,
    shrink: shrink.noop
  });
