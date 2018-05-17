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

  it('should sort imports when there are native and type modules', () => {
    //given
    const input = `import logger from 'winston';
import KoaRouter from 'koa-router';
import uuid from 'uuid';

import type { Car } from '../cars';`;

    //when
    const organizedImports = organizeImports(input);

    //then
    expect(organizedImports).toEqual(`import KoaRouter from 'koa-router';
import uuid from 'uuid';
import logger from 'winston';

import type { Car } from '../cars';`);
  });

  it('should sort imports when there are native, local and type modules', () => {
    //given
    const input = `import logger from 'winston';
import KoaRouter from 'koa-router';
import uuid from 'uuid';

import { contactInfo } from '../contract';
import { carInfo } from '../cars';

import type { Car } from '../cars';`;

    //when
    const organizedImports = organizeImports(input);

    //then
    expect(organizedImports).toEqual(`import KoaRouter from 'koa-router';
import uuid from 'uuid';
import logger from 'winston';

import { carInfo } from '../cars';
import { contactInfo } from '../contract';

import type { Car } from '../cars';`);
  });
});
