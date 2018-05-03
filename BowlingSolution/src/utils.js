// @flow

import { addIndex, drop, flip, map } from 'ramda';

export const flippedDrop = flip(drop);

export const mapIndexed = addIndex(map());
