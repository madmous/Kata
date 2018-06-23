type Log<A> = (nbr: number) => (val: A) => A;

const log: Log<any> = nbr => val => {
  console.log(`log ${nbr}: `, val);
  return val;
};

type Adder = (a: number, b: number) => number;

export { log as default };
