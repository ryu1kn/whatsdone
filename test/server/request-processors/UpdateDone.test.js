
const UpdateDoneRequestProcessor = require('../../../src/server/request-processors/UpdateDone');
const ServiceLocator = require('../../../src/server/ServiceLocator');

describe('Server UpdateDoneRequestProcessor', () => {

  it('bridges http request/response pair to UpdateDone command', () => {
    const updateDoneCommand = {execute: sinon.stub().returns(Promise.resolve({UPDATED_DONE_ITEM: '..'}))};
    ServiceLocator.load({
      createUpdateDoneCommand: () => updateDoneCommand
    });
    const processor = new UpdateDoneRequestProcessor();

    const req = {
      session: {userId: 'USER_ID'},
      params: {id: 'DONE_ID'},
      body: {DONE_DATA: '..'}
    };
    return processor.process(req).then(response => {
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

