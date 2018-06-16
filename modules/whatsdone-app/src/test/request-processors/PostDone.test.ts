import PostDoneRequestProcessor from '../../lib/request-processors/PostDone';
import ServiceLocator from '../../lib/ServiceLocator';
import {expect} from '../helper/TestUtils';
import ServiceFactory from '../../lib/ServiceFactory';
import {request} from '../helper/NormalisedRequestData';
import * as td from 'testdouble';
import CreateDoneCommand from '../../lib/commands/CreateDone';

describe('Server PostDoneRequestProcessor', () => {

  it('saves a done item', () => {
    const done = {doneThing: 'SOMETHING', date: 'DATE'};
    const session = {userId: 'USER_ID', username: 'USER_NAME'};

    const createDoneCommand = td.object('execute') as CreateDoneCommand;
    td.when(createDoneCommand.execute(done, session.userId))
      .thenResolve({doneThing: 'SOMETHING', userId: 'USER_ID'});
    ServiceLocator.load({createCreateDoneCommand: () => createDoneCommand} as ServiceFactory);
    const processor = new PostDoneRequestProcessor();

    const req = Object.assign({}, request, {body: done});
    return processor.process(req, session).then(response => {
      expect(response).to.eql({
        statusCode: '200',
        headers: {'Content-Type': 'application/json'},
        body: '{\"doneThing\":\"SOMETHING\",\"userId\":\"USER_ID\",\"username\":\"USER_NAME\"}'
      });
    });
  });

});
