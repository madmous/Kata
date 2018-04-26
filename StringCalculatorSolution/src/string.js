// @flow

type Split = (delimiter: string) => (string: string) => string[];
export const split: Split = delimiter => string => string.split(delimiter);

type IndexOf = (searchString: string) => (string: string) => number;
export const indexOf: IndexOf = searchString => string => string.indexOf(searchString);

type Repalce = (separator: string) =>(delimeter: string | RegExp) => (characters: string) => string;
export const replace: Repalce = separator => delimeter => characters => characters.replace(delimeter, separator);

type CharAt = (index: number) => (characters: string) => string;
export const charAt: CharAt = index => characters => characters.charAt(index);