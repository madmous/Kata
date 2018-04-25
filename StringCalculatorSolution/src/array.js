// @flow

type UnaryFonction<A, B> = (a: A) => B;

type TwoArgumentFonction<A, B, C> = (a: A, b: B) => C;

type Reduce<A> = (f: TwoArgumentFonction<A, A, A>) => (initialValue: A) => (array: A[]) => A;
export const reduce: Reduce<*> = f => initialValue => array => array.reduce(f, initialValue);

type Map<A,B> = (
  f: UnaryFonction<A, B>
) => (array: A[]) => B[];
export const map: Map<*,*> = f => array => array.map(f);

// export function map<A,B>(f: UnaryFonction<A,B>) {
//   return (array: A[]): B[] => {
//     return array.map(f);
//   };
// }

type Filter<A> = (
  predicate: UnaryFonction<A, boolean>
) => (array: A[]) => A[];
export const filter: Filter<*> = predicate => array =>
  array.filter(predicate);


type FindIndex<A> = (
  predicate: UnaryFonction<A, boolean>
) => (array: A[]) => number;
export const findIndex: FindIndex<*> = f => array => array.findIndex(f);