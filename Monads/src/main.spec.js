/** @flow */

import { Left, Right, Either, fromNullable, tryCatch } from './main';

describe('Left', () => {
  it('should return same type on map', () => {
    // given
    const left = new Left(2);

    // when
    const result = left.map(x => x * 2);

    // then
    expect(result).toBeInstanceOf(Left);
  });

  it('should do nothing when map is called', () => {
    // given
    const left = new Left(2);

    // when
    const result = left.map(x => x * 2);

    // then
    expect(result).toEqual(left);
  });

  it('should do work with any type', () => {
    // given
    const left = new Left('string');

    // when
    const result = left.map(x => x + '2');

    // then
    expect(result).toEqual(left);
  });

  it('should apply function on fold', () => {
    // given
    const left = new Left(1);

    // when
    const result = left.fold(x => x * 2, x => x);

    // then
    expect(result).toEqual(2);
  });
});

describe('Right', () => {
  it('should apply map function', () => {
    // given
    const right = new Right(2);

    // when
    const result = right.map(x => x * 2);

    // then
    expect(result).toEqual(new Right(4));
  });

  it('should return same type on map', () => {
    // given
    const right = new Right(2);

    // when
    const result = right.map(x => x * 2);

    // then
    expect(result).toBeInstanceOf(Right);
  });

  it('should be possible to chain map functions', () => {
    // given
    const right = new Right(2);

    // when
    const result = right.map(x => x * 2).map(x => x + 2);

    // then
    expect(result).toEqual(new Right(6));
  });

  it('should get the value', () => {
    // given
    const right = new Right(2);

    // when
    const result = right
      .map(x => x * 2)
      .map(x => x + 2)
      .get();

    // then
    expect(result).toEqual(6);
  });

  it('should apply flatmap function', () => {
    // given
    const right = new Right(2);

    // when
    right
      .flatmap(x => x * 2)
      // then
      .map(result => expect(result).toEqual(new Right(4)));
  });
});

describe('Either', () => {
  it('should fold on either Right', () => {
    // given
    const either = new Either(new Right(2));

    // then
    const expected = (x) => expect(x).toEqual(2);

    // when
    either.fold(x => x, expected);
  });

  it('should fold on either Left', () => {
    // given
    const either = new Either(new Left(null));

    // then
    const expected = (x) => expect(x).toEqual(null);

    // when
    either.fold(expected, x => x);
  });

  it('should accept on construct wether Left or Right', () => {
    // given
    const either = new Either(new Left(2));

    // when
    let value;
    either.fold(x => (value = x * 2), x => (value = x));

    // then
    expect(value).toEqual(4);
  });

  it('should apply map function', () => {
    // given
    const either = new Either(new Right(2));

    // when
    const result = either.map(x => x * 2);

    // then
    expect(result).toEqual(new Either(new Right(4)));
  });

  it('should not apply map function on Left type', () => {
    // given
    const either = new Either(new Left(2));

    // when
    const result = either.map(x => x * 2);

    // then
    expect(result).toEqual(new Either(new Left(2)));
  });
});

describe('fromNullable', () => {
  it('should return left when value is nullable', () => {
    // given
    const value = undefined;

    // when
    const result = fromNullable(value);

    // then
    expect(result).toBeInstanceOf(Left);
    expect(result).toEqual(new Left(value));
  });
  
  it('should return right when value is not nullable', () => {
    // given
    const value = 42;
    
    // when
    const result = fromNullable(value);
    
    // then
    expect(result).toBeInstanceOf(Right);
    expect(result).toEqual(new Right(value));
  });
});

describe('tryCatch', () => {
  it('should execute when no exception', () => {
  // given
  const pected = 2;
  const fn = () => pected;

  // when
  const result = tryCatch(fn);

  // then
  expect(result).toBeInstanceOf(Right);
  expect(result).toEqual(new Right(pected));
  });

  it('should not execute when there is an exception', () => {
    // given
    const error = new Error('There is a problem');
    const fn = () => {
      throw error;
    };
  
    // when
    const result = tryCatch(fn);
  
    // then
    expect(result).toBeInstanceOf(Left);
    expect(result).toEqual(new Left(error));
  });

  xit('should not execute when async fail', async () => {
    // given
    const error = new Error('There is a problem');
    const fn = () => Promise.reject(error);
  
    // when
    const result = tryCatch(fn);
    console.log('result', result);
  
    // then
    expect(result).toBeInstanceOf(Left);
    expect(result).toEqual(new Left(error));
  });
});