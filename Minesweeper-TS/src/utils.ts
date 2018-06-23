import { concat, reduce } from 'lodash/fp';

type Flatten<T> = (arg: ReadonlyArray<ReadonlyArray<T>>) => ReadonlyArray<T>;
export const flatten = <T>(
  arg: ReadonlyArray<ReadonlyArray<T>>
): ReadonlyArray<T> =>
  reduce((acc: ReadonlyArray<T>, row: ReadonlyArray<T>) => [...acc, ...row])(
    []
  )(arg);
