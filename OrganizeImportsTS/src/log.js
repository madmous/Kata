// @flow

type Log<A> = (nbr: number) => (val: A) => A;
const log: Log<*> = nbr => val => {
  console.log(`log ${nbr}: `, val);
  return val;
};

export { log as default };