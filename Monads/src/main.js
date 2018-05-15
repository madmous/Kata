/** @flow */

type MapFunction<T> = (value: T) => T;
export class Left<T> {
  value: T;

  constructor(value: T) {
    this.value = value;
  }

  map(f: MapFunction<T>): Left<T> {
    return new Left(this.value);
  }

  fold(f: MapFunction<T>, _: MapFunction<T>): T {
    return f(this.value);
  }

  inspect() {
    return `Left(${String(this.value)})`;
  }
}

export class Right<T> {
  value: T;

  constructor(value: T) {
    this.value = value;
  }

  map(f: MapFunction<T>): Right<T> {
    const right = new Right(f(this.value));
    return right;
  }


  fold(_: MapFunction<T>, g: MapFunction<T>): T {
    return g(this.value);
  }

  get(): T {
    return this.value;
  }

  inspect() {
    return `Right(${String(this.value)})`;
  }
}

type LeftOrRight<T> = Left<T>|Right<T>;
export class Either<T> {
  value: LeftOrRight<T>;

  constructor(value: LeftOrRight<T>) {
    this.value = value;
  }

  fold(f: (value: T) => T, g: (value: T) => T): void {
    this.value.fold(f, g);
  }

  map(f: (value: T) => T): Either<T> {
    return new Either(this.value.map(f));
  }

  inspect() {
    return `Either(${this.value.inspect()})`;
  }
}

// Ideas: change to Result class and add a flatmap
type FromNullable = <T: any>(value: T) => LeftOrRight<T>;
export const fromNullable: FromNullable = (value) => {
  return value
    ? new Right(value)
    : new Left(value);
};

type TryCatch = <T>(f: () => T) => LeftOrRight<T>;
export const tryCatch: TryCatch = f => {
  try {
    return new Right(f());
  } catch(e) {
    return new Left(e);
  }
};