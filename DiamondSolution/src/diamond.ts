import { flow, join } from 'lodash/fp';

import createRow, { addRemainingRows } from './row';

// TYPE(S)

type Diamond = string;
type Base = string[];

// MAIN

type CreateDiamond = (letter: string) => Diamond;
const createDiamond: CreateDiamond = letter =>
  flow(createDiamondBase, addRemainingRows(letter), join('\n'))(letter);

export default createDiamond;

type CreateDiamondBase = (letter: string) => Base;
const createDiamondBase: CreateDiamondBase = letter => [createRow(letter)];
