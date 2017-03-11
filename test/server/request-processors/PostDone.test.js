
const PostDoneRequestProcessor = require('../../../src/server/request-processors/PostDone');
const ServiceLocator = require('../../../src/server/ServiceLocator');

describe('Server PostDoneRequestProcessor', () => {

  it('saves a done item', () => {
    const createDoneCommand = {execute: sinon.stub().returns(Promise.resolve('CREATED_DONE'))};
    ServiceLocator.load({createCreateDoneCommand: () => createDoneCommand});
    const processor = new PostDoneRequestProcessor();

    const request = {
      session: {userId: 'USER_ID'},
      body: {SOME_DATA: '..'}
    };
    return processor.process(request).then(response => {
      expect(response).to.eql({
        statusCode: '200',
        headers: {'Content-Type': 'application/json'},
        body: '"CREATED_DONE"'
      });
      expect(createDoneCommand.execute).to.have.been.calledWith({
        data: {SOME_DATA: '..'},
        userId: 'USER_ID'
      });
    });
  });

});
