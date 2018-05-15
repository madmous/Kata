/** @flow */
import { flow, join, last, map, split } from 'lodash/fp';

// import log from './log';

type importsByModule = { name: string, import: string };

type GroupImportsByModule = (strings: string[]) => importsByModule[];
const groupImportsByModule: GroupImportsByModule = strings => {
  return map(v => {
    const module = last(split(' ')(v));

    return {
      name: module,
      import: v,
    };
  })(strings);
};

type OrderAlphabetically = (
  a: importsByModule,
  b: importsByModule
) => 1 | -1 | 0;
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
  fn: (importsByModule, importsByModule) => 1 | -1 | 0
) => (importsByModule[]) => importsByModule[];
const sort: Sort = fn => importsByModule =>
  Array.from(importsByModule, x => x).sort(fn);

type OrganizeImports = (input: string) => string;
const organizeImports: OrganizeImports = input => {
  const organizedImports = flow(
    split('\n'),
    groupImportsByModule,
    sort(orderAlphabetically),
    map(v => v.import),
    join('\n')
  )(input);

  return organizedImports;
};

export { organizeImports as default };
