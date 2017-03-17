
const DeleteDoneRequestProcessor = require('../../../src/server/request-processors/DeleteDone');
const ServiceLocator = require('../../../src/server/ServiceLocator');

describe('Server DeleteDoneRequestProcessor', () => {

  it('deletes a done item', () => {
    const doneRepository = {remove: sinon.stub().returns(Promise.resolve())};
    ServiceLocator.load({createDoneRepository: () => doneRepository});
    const processor = new DeleteDoneRequestProcessor();

    const request = {
      session: {userId: 'USER_ID'},
      params: {id: 'DONE_ID'}
    };
    return processor.process(request).then(response => {
      expect(response).to.eql({
        statusCode: '200'
      });
      expect(doneRepository.remove).to.have.been.calledWith('DONE_ID', 'USER_ID');
    });
  });

  it('propagates error', () => {
    ServiceLocator.load({
      createDoneRepository: () => ({
        remove: () => Promise.reject(new Error('UNEXPECTED_ERROR'))
      })
    });
    const processor = new DeleteDoneRequestProcessor();

    const request = {
      params: {},
      session: {}
    };
    return processor.process(request).then(
      throwError,
      e => {
        expect(e).to.have.property('message', 'UNEXPECTED_ERROR');
      }
    );
  });

});

