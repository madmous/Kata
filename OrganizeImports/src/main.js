/** @flow */
import {
  filter,
  flow,
  head,
  includes,
  join,
  last,
  split,
  startsWith,
  tail,
} from 'lodash/fp';

// import log from './log';

type ModulesType = 'nativeModules' | 'typeModules' | 'localModules';

type ModuleType = 'nativeModule' | 'typeModule' | 'localModule';

type Import = { name: string, description: string, type: ModuleType };

type ImportsByModuleType = { [key: ModulesType]: Import[] };

type IsLocalModule = (string: string) => boolean;
const isLocalModule: IsLocalModule = string =>
  startsWith("'/")(string) || startsWith("'..")(string);

type IsTypeModule = (string: string) => boolean;
const isTypeModule: IsTypeModule = string => includes('type')(string);

type GetImportType = (
  description: string
) => (description: string) => ModuleType;
const getImportType: GetImportType = description => name => {
  if (isTypeModule(description)) {
    return 'typeModule';
  } else if (isLocalModule(name)) {
    return 'localModule';
  } else {
    return 'nativeModule';
  }
};

type GroupImportByModule = (strings: string) => Import;
const groupImportByModule: GroupImportByModule = string => {
  const module = last(split(' ')(string));
  const type = getImportType(string)(module);

  return {
    name: module,
    description: string,
    type,
  };
};

type ToNextBuildOutputAcc = (acc: string[]) => (imports: Import[]) => string[];
const toNextBuildOutputAcc: ToNextBuildOutputAcc = acc => imports => {
  const headImport = head(imports);
  if (typeof headImport === 'string') {
    return [...acc, ''];
  } else {
    const { description } = head(imports);

    return [...acc, description];
  }
};

type BuildOutput = (acc: string[]) => (imports: Import[]) => string[];
const buildOutput: BuildOutput = acc => imports => {
  if (imports.length === 0) {
    return acc;
  } else {
    const nextAcc = toNextBuildOutputAcc(acc)(imports);
    const nextImports = tail(imports);

    return buildOutput(nextAcc)(nextImports);
  }
};

type ConcatModules = (
  importsByModuleType: ImportsByModuleType
) => Array<Import | ''>;
const concatModules: ConcatModules = importsByModuleType => {
  const { localModules, nativeModules, typeModules } = importsByModuleType;

  if (nativeModules.length > 0 && typeModules.length > 0 && localModules.length > 0) {
    return [...nativeModules, '', ...localModules ,'', ...typeModules];
  } else if (nativeModules.length > 0 && typeModules.length > 0) {
    return [...nativeModules, '', ...typeModules];
  } else {
    return [...nativeModules];
  }
};

type OrderAlphabetically = (a: Import, b: Import) => 1 | -1 | 0;
const orderAlphabetically: OrderAlphabetically = (a, b) => {
  if (a.name < b.name) {
    return -1;
  } else if (a.name > b.name) {
    return 1;
  } else {
    return 0;
  }
};

type Sort = (fn: (Import, Import) => 1 | -1 | 0) => (Import[]) => Import[];
const sort: Sort = fn => importsByModule =>
  Array.from(importsByModule, x => x).sort(fn);

type SortImportsByModuleType = (
  importsByModuleType: ImportsByModuleType
) => ImportsByModuleType;
const sortImportsByModuleType: SortImportsByModuleType = importsByModuleType => {
  const { typeModules, localModules, nativeModules } = importsByModuleType;

  const orderedTypeModules = sort(orderAlphabetically)(typeModules);
  const orderedNativeModules = sort(orderAlphabetically)(nativeModules);
  const orderedLocalModules = sort(orderAlphabetically)(localModules);

  return {
    typeModules: orderedTypeModules,
    nativeModules: orderedNativeModules,
    localModules: orderedLocalModules,
  };
};

type ToNextGroupImportsByModuleTypeAcc = (
  acc: ImportsByModuleType
) => (string: string) => ImportsByModuleType;
const toNextGroupImportsByModuleTypeAcc: ToNextGroupImportsByModuleTypeAcc = acc => string => {
  const importByModule = groupImportByModule(string);

  switch (importByModule.type) {
    case 'typeModule':
      return {
        ...acc,
        typeModules: [...acc.typeModules, importByModule],
      };
    case 'localModule':
      return {
        ...acc,
        localModules: [...acc.localModules, importByModule],
      };
    default:
      return {
        ...acc,
        nativeModules: [...acc.nativeModules, importByModule],
      };
  }
};

type GroupImportsByModuleType = (
  acc: ImportsByModuleType
) => (strings: string[]) => ImportsByModuleType;
const groupImportsByModuleType: GroupImportsByModuleType = acc => strings => {
  if (strings.length === 0) {
    return acc;
  } else {
    const nextAcc = toNextGroupImportsByModuleTypeAcc(acc)(head(strings));
    const nextStrings = tail(strings);

    return groupImportsByModuleType(nextAcc)(nextStrings);
  }
};

type OrganizeImports = (input: string) => string;
const organizeImports: OrganizeImports = input => {
  const RENTURN_SYMBOL = '\n';

  return flow(
    split(RENTURN_SYMBOL),
    filter(v => v !== ''),
    groupImportsByModuleType({
      typeModules: [],
      nativeModules: [],
      localModules: [],
    }),
    sortImportsByModuleType,
    concatModules,
    buildOutput([]),
    join(RENTURN_SYMBOL)
  )(input);
};

export { organizeImports as default };
