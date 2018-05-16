/** @flow */
import { flow, join, last, map, split } from 'lodash/fp';

// import log from './log';

type ImportsByModule = { import: string, name: string };

type GroupImportsByModule = (char: string) => ImportsByModule;
const groupImportsByModule: GroupImportsByModule = char => {
  const module = last(split(' ')(char));

  return {
    import: char,
    name: module
  };
};

type OrderAlphabetically = (a: ImportsByModule, b: ImportsByModule) => 1 | -1 | 0;
const orderAlphabetically: OrderAlphabetically = (a, b) => {
  if (a.name < b.name) {
    return -1;
  } else if (a.name > b.name) {
    return 1;
  } else {
    return 0;
  }
};

type Sort = (
  fn: (a: ImportsByModule, b: ImportsByModule) => 1 | -1 | 0
) => (importsByModule: ImportsByModule[]) => ImportsByModule[];
const sort: Sort = fn => importsByModule => [...importsByModule].sort(fn);

type OrganizeImports = (input: string) => string;
const organizeImports: OrganizeImports = input => {
  const organizedImports = flow(
    split('\n'),
    map(groupImportsByModule),
    sort(orderAlphabetically),
    map(v => v.import),
    join('\n')
  )(input);

  return organizedImports;
};

export { organizeImports as default };
