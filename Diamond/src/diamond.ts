// TYPE(S)

type Diamond = string;

// MAIN

type CreateDiamond = (letter: string) => Diamond;
const createDiamond: CreateDiamond = letter => {
  throw new Error('Not implemented yet');
};

export default createDiamond;
