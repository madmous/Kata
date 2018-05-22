/** @flow */
import {
  endsWith,
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

import log from './log';

type ModulesType =
  | 'nativeModules'
  | 'typeModules'
  | 'localModules'
  | 'workspaceModules';

type ModuleType =
  | 'nativeModule'
  | 'typeModule'
  | 'localModule'
  | 'workspaceModule';

type Import = { name: string, description: string, type: ModuleType };

type ImportsByModuleType = { [key: ModulesType]: Import[] };

type IsWorkspaceModule = (string: string) => boolean;
const isWorkspaceModule: IsWorkspaceModule = string => startsWith("'@")(string);

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
  } else if (isWorkspaceModule(name)) {
    return 'workspaceModule';
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

const addNativeModules = modules => res => [...modules];

const addRest = modules => res => {
  if (modules.length > 0) {
    if (res.length > 0) {
      return [...res, '', ...modules];
    } else {
      return modules;
    }
  } else {
    return res;
  }
};

type ConcatModules = (
  importsByModuleType: ImportsByModuleType
) => Array<Import | ''>;
const concatModules: ConcatModules = importsByModuleType => {
  const {
    localModules,
    nativeModules,
    typeModules,
    workspaceModules,
  } = importsByModuleType;

  return flow(
    addNativeModules(nativeModules),
    addRest(workspaceModules),
    addRest(localModules),
    addRest(typeModules)
  )([]);
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

type Sort = (
  fn: (Import, Import) => 1 | -1 | 0
) => (imports: Import[]) => Import[];
const sort: Sort = fn => importsByModule =>
  Array.from(importsByModule, v => v).sort(fn);

type SortImportsByModuleType = (
  importsByModuleType: ImportsByModuleType
) => ImportsByModuleType;
const sortImportsByModuleType: SortImportsByModuleType = importsByModuleType => {
  const {
    typeModules,
    localModules,
    nativeModules,
    workspaceModules,
  } = importsByModuleType;

  const orderedTypeModules = sort(orderAlphabetically)(typeModules);
  const orderedNativeModules = sort(orderAlphabetically)(nativeModules);
  const orderedLocalModules = sort(orderAlphabetically)(localModules);
  const orderedWorkspaceModules = sort(orderAlphabetically)(workspaceModules);

  return {
    typeModules: orderedTypeModules,
    nativeModules: orderedNativeModules,
    localModules: orderedLocalModules,
    workspaceModules: orderedWorkspaceModules,
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
    case 'workspaceModule':
      return {
        ...acc,
        workspaceModules: [...acc.workspaceModules, importByModule],
      };
    case 'nativeModule':
      return {
        ...acc,
        nativeModules: [...acc.nativeModules, importByModule],
      };
    default:
      throw new Error(`${importByModule.type} is not a supported module`);
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

type IsAnImportDescription = (string: string) => boolean;
const isAnImportDescription: IsAnImportDescription = string =>
  startsWith('import')(string) && endsWith("';")(string);

type ToNextCurrentImport = (
  currentImport: string[]
) => (string: string) => string[];
const toNextCurrentImport: ToNextCurrentImport = currentImport => string => {
  const joinedImport = join('')(currentImport);

  if (isAnImportDescription(joinedImport) || isAnImportDescription(string)) {
    return [];
  } else {
    return [...currentImport, string];
  }
};

type ToNextFormatAcc = (
  acc: string[]
) => (currentImport: string[]) => (string: string) => string[];
const toNextFormatAcc: ToNextFormatAcc = acc => currentImport => string => {
  if (isAnImportDescription(string)) {
    return [...acc, string];
  } else {
    const joinedImport = join('')([...currentImport, string]);

    if (isAnImportDescription(joinedImport)) {
      return [...acc, joinedImport];
    } else {
      return acc;
    }
  }
};

type Format = (
  acc: string[]
) => (currentImport: string[]) => (strings: string[]) => string[];
const format: Format = acc => currentImport => strings => {
  if (strings.length === 0) {
    return acc;
  } else {
    const headString = head(strings);

    const nextFormatAcc = toNextFormatAcc(acc)(currentImport)(headString);
    const nextCurrentImport = toNextCurrentImport(currentImport)(headString);
    const nextStrings = tail(strings);

    return format(nextFormatAcc)(nextCurrentImport)(nextStrings);
  }
};

type OrganizeImports = (input: string) => string;
const organizeImports: OrganizeImports = input => {
  const RETURN_SYMBOL = '\n';

  return flow(
    split(RETURN_SYMBOL),
    filter(v => v !== ''),
    format([])([]),
    groupImportsByModuleType({
      typeModules: [],
      nativeModules: [],
      localModules: [],
      workspaceModules: [],
    }),
    sortImportsByModuleType,
    concatModules,
    buildOutput([]),
    join(RETURN_SYMBOL)
  )(input);
};

export { organizeImports as default };
