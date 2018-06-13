
import UpdateDoneRequestProcessor = require('../../lib/request-processors/UpdateDone');
import ServiceLocator from '../../lib/ServiceLocator';
import {expect} from '../TestUtils';
import sinon = require('sinon');

describe('Server UpdateDoneRequestProcessor', () => {

  it('bridges http request/response pair to UpdateDone command', () => {
    const updateDoneCommand = {execute: sinon.stub().returns(Promise.resolve({UPDATED_DONE_ITEM: '..'}))};
    ServiceLocator.load({
      createUpdateDoneCommand: () => updateDoneCommand
    });
    const processor = new UpdateDoneRequestProcessor();

    const request = {
      params: {id: 'DONE_ID'},
      body: {DONE_DATA: '..'}
    };
    const session = {userId: 'USER_ID'};
    return processor.process(request, session).then(response => {
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

