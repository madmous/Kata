// @flow
import { maybe as Maybe, result as Result } from 'folktale';

type Into<A, B, C> = (f: (x: A) => B) => (value: C) => C;

type ToMaybeWhenEmpty = (value: string | []) => Maybe<string | []>;

type ApplyToMaybeString<A, B> = (f: (x: A) => B) => (numbers: A) => Maybe<B>;

const into: Into<*, *, *> = f => value => value.map(f);

const toMaybeWhenEmpty: ToMaybeWhenEmpty = value =>
  value.length === 0 ? Maybe.Nothing() : Maybe.Just(value);

const applyToMaybeString: ApplyToMaybeString<*, *> = f => value =>
  value.length === 0 ? Maybe.Nothing() : Maybe.Just(f(value));

export { Maybe, Result, into, applyToMaybeString, toMaybeWhenEmpty };
