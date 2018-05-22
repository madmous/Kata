/** @flow */

import organizeImports from './main';

describe('Organize imports', () => {
  it('should return the same import when there is only one import', () => {
    //given
    const input = `import KoaRouter from 'koa-router';`;

    //when
    const organizedImports = organizeImports(input);

    //then
    expect(organizedImports).toEqual(`import KoaRouter from 'koa-router';`);
  });

  it('should flatten imports on multiple lines', () => {
    //given
    const input = `import {
logger
} from 'winston';`;

    //when
    const organizedImports = organizeImports(input);

    //then
    expect(organizedImports).toEqual(`import {logger} from 'winston';`);
  });

  xit('should flatten imports on multiple lines when there are tabulations', () => {
    //given
    const input = `import {
  logger
} from 'winston';`;

    //when
    const organizedImports = organizeImports(input);

    //then
    expect(organizedImports).toEqual(`import {logger} from 'winston';`);
  });

  it('should sort imports when there are only native imports', () => {
    //given
    const input = `import logger from 'winston';
import KoaRouter from 'koa-router';
import uuid from 'uuid';`;

    //when
    const organizedImports = organizeImports(input);

    //then
    expect(organizedImports).toEqual(`import KoaRouter from 'koa-router';
import uuid from 'uuid';
import logger from 'winston';`);
  });

  it('should sort imports when there are local modules', () => {
    //given
    const input = `import { contactInfo } from '../contact';
import { carInfo } from '../cars';`;

    //when
    const organizedImports = organizeImports(input);

    //then
    expect(organizedImports).toEqual(`import { carInfo } from '../cars';
import { contactInfo } from '../contact';`);
  });

  it('should sort imports when there are native and type modules', () => {
    //given
    const input = `import logger from 'winston';
import KoaRouter from 'koa-router';
import uuid from 'uuid';

import type {
Car
} from '../cars';`;

    //when
    const organizedImports = organizeImports(input);

    //then
    expect(organizedImports).toEqual(`import KoaRouter from 'koa-router';
import uuid from 'uuid';
import logger from 'winston';

import type {Car} from '../cars';`);
  });

  it('should sort imports when there are native, local, type modules and workspaces', () => {
    //given
    const input = `import logger from 'winston';
import uuid from '@tech/http';
import KoaRouter from 'koa-router';

import { carInfo } from '../cars';

import type { Car } from '../cars';`;

    //when
    const organizedImports = organizeImports(input);

    //then
    expect(organizedImports).toEqual(`import KoaRouter from 'koa-router';
import logger from 'winston';

import uuid from '@tech/http';

import { carInfo } from '../cars';

import type { Car } from '../cars';`);
  });
});
