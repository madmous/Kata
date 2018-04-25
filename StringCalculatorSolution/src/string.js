// @flow

type Split = (delimiter: string) => (string: string) => string[];
export const split: Split = delimiter => string => string.split(delimiter);

type IndexOf = (searchString: string) => (string: string) => number;
export const indexOf: IndexOf = searchString => string => string.indexOf(searchString);