import PostDoneRequestProcessor from '../../lib/request-processors/PostDone';
import ServiceLocator from '../../lib/ServiceLocator';
import ServiceFactory from '../../lib/ServiceFactory';
import {request} from '../helper/NormalisedRequestData';
import * as td from 'testdouble';
import CreateDoneCommand from '../../lib/commands/CreateDone';
import {deepStrictEqual} from 'assert';

describe('Server PostDoneRequestProcessor', () => {

  it('saves a done item', async () => {
    const done = {doneThing: 'SOMETHING', date: 'DATE'};
    const session = {userId: 'USER_ID', username: 'USER_NAME'};

    const createDoneCommand = td.object('execute') as CreateDoneCommand;
    td.when(createDoneCommand.execute(done, session.userId))
      .thenResolve({doneThing: 'SOMETHING', userId: 'USER_ID'});
    ServiceLocator.load({createCreateDoneCommand: () => createDoneCommand} as ServiceFactory);
    const processor = new PostDoneRequestProcessor();

    const req = Object.assign({}, request, {body: done});
    const response = await processor.process(req, session);
    deepStrictEqual(response, {
      statusCode: '200',
      headers: {'Content-Type': 'application/json'},
      body: '{\"doneThing\":\"SOMETHING\",\"userId\":\"USER_ID\",\"username\":\"USER_NAME\"}'
    });
  });
});
