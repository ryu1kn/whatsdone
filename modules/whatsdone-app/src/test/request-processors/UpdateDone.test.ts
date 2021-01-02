import {expect} from 'chai';
import * as td from 'testdouble';
import UpdateDoneRequestProcessor from '../../lib/request-processors/UpdateDone';
import ServiceLocator from '../../lib/ServiceLocator';
import ServiceFactory from '../../lib/ServiceFactory';
import {request, session} from '../helper/NormalisedRequestData';
import UpdateDoneCommand from '../../lib/commands/UpdateDone';

describe('Server UpdateDoneRequestProcessor', () => {

  it('bridges http request/response pair to UpdateDone command', async () => {
    const doneDiff = {doneThing: '..'};
    const updateDoneCommand = td.object('execute') as UpdateDoneCommand;
    td.when(updateDoneCommand.execute(doneDiff, 'DONE_ID', 'USER_ID'))
      .thenResolve({doneThing: 'UPDATED_DONE_THING'});
    ServiceLocator.load({
      createUpdateDoneCommand: () => updateDoneCommand
    } as ServiceFactory);
    const processor = new UpdateDoneRequestProcessor();

    const req = Object.assign({}, request, {body: doneDiff});
    const response = await processor.process(req, session);
    expect(response).to.eql({
      statusCode: '200',
      headers: {
        'Content-Type': 'application/json',
        'Cache-Control': 'no-cache'
      },
      body: '{"doneThing":"UPDATED_DONE_THING"}'
    });
  });

});

