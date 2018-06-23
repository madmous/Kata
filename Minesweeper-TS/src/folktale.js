// @flow
import { maybe as Maybe, result as Result } from 'folktale';

type Into<A, B, C> = (f: (x: A) => B) => (value: C) => C;
export const into: Into<any, any, any> = f => value => value.map(f);

type ApplyToMaybeString<A, B> = (f: (x: A) => B) => (numbers: A) => Maybe<B>;
export const applyToMaybeString: ApplyToMaybeString<any, any> = f => value =>
  value.length === 0 ? Maybe.Nothing() : Maybe.Just(f(value));

type GetValueOrFail<A,B> = (result: Result<A,B>) => A;
export const getValueOrFail: GetValueOrFail<any,any> = result => {
  const value = result.matchWith({
    Ok:    ({ value }) => value,
    Error: ({ value }) => {
      throw new Error(value);
    },
  });

  return value;
};

export { Maybe, Result };
