import { drop } from 'lodash/fp';
import { addIndex, map } from 'ramda';

type FlippedDrop = <T>(array: T[]) => (dropCount: number) => T[];
export const flippedDrop: FlippedDrop = array => dropCount => drop(dropCount)(array);

export const mapIndexed = addIndex(map);
