// @flow

declare module 'folktale' {
  declare class maybe<B> {
    static Just(b: B): maybe<B>;
    static Nothing(): maybe<B>;

    static empty(): maybe<B>;
    static of(b: B): maybe<B>;

    static fromNullable<B>(a: ?B): maybe<B>;
    static fromResult<A>(aResult: result<A, B>): maybe<B>;

    concat(aMaybe: maybe<B>): maybe<B>;
    getOrElse(b: B): maybe<B>;

    chain<C>(f: (b: B) => maybe<C>): maybe<C>;
    map<C>(f: (b: B) => C): maybe<C>;
    matchWith<C>({
      Just: ({ value: B }) => C,
      Nothing: () => C,
    }): C;
    orElse<C>(f: (a: B) => maybe<C>): maybe<C>;
  }

  declare class result<A, B> {
    static Error(a: A): result<A, B>;
    static Ok(b: B): result<A, B>;

    static fromMaybe(aMaybe: maybe<B>, a: A): result<A, B>;
    static fromNullable(a: ?null, a: A): result<A, B>;

    concat(aResult: result<A, B>): result<A, B>; // care aResult needs to be a semi group
    getOrElse(a: A): result<A, B>;
    merge(): A | B;

    chain<C>(f: (b: B) => result<A, C>): result<A, C>; // experimental
    map<C>(f: (b: B) => C): result<A, C>;
    mapError<C>(f: (a: A) => C): result<C, B>; // experimental
    matchWith<C>({
      Ok: ({ value: B }) => C,
      Error: ({ value: A }) => C,
    }): C;
    orElse<C>(f: (a: A) => result<C, B>): result<C, B>;
  }

  declare module.exports: {
    maybe: typeof maybe,
    result: typeof result,
  };
}
