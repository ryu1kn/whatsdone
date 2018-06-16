import UpdateDoneRequestProcessor from '../../lib/request-processors/UpdateDone';
import ServiceLocator from '../../lib/ServiceLocator';
import {expect} from '../helper/TestUtils';
import sinon = require('sinon');
import ServiceFactory from '../../lib/ServiceFactory';
import {request, session} from '../helper/NormalisedRequestData';

describe('Server UpdateDoneRequestProcessor', () => {

  it('bridges http request/response pair to UpdateDone command', () => {
    const updateDoneCommand = {execute: sinon.stub().returns(Promise.resolve({UPDATED_DONE_ITEM: '..'}))};
    ServiceLocator.load({
      createUpdateDoneCommand: () => updateDoneCommand
    } as ServiceFactory);
    const processor = new UpdateDoneRequestProcessor();

    const req = Object.assign({}, request, {
      body: {DONE_DATA: '..'}
    });
    return processor.process(req, session).then(response => {
      expect(response).to.eql({
        statusCode: '200',
        headers: {
          'Content-Type': 'application/json',
          'Cache-Control': 'no-cache'
        },
        body: '{"UPDATED_DONE_ITEM":".."}'
      });
      expect(updateDoneCommand.execute).to.have.been.calledWith({
        doneId: 'DONE_ID',
        userId: 'USER_ID',
        data: {DONE_DATA: '..'}
      });
    });
  });

});

