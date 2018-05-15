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

  it('should sort imports when there are 2 imports', () => {
    //given
    const input = `import logger from 'winston';
import KoaRouter from 'koa-router';`;

    //when
    const organizedImports = organizeImports(input);

    //then
    expect(organizedImports).toEqual(`import KoaRouter from 'koa-router';
import logger from 'winston';`);
  });

  it('should sort imports when there are more than 2 imports', () => {
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
});
