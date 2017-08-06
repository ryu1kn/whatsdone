
const ServiceLocator = require('../../lib/ServiceLocator');
const SignoutRequestProcessor = require('../../lib/request-processors/Signout');

describe('Server SignoutRequestProcessor', () => {

  it('removes session', () => {
    const sessionRepository = {remove: sinon.stub().returns(Promise.resolve())};
    ServiceLocator.load({createSessionRepository: () => sessionRepository});
    const processor = new SignoutRequestProcessor();
    const request = {};
    const session = {id: 'SESSION_ID'};

    return processor.process(request, session).then(response => {
      expect(response).to.eql({
        statusCode: '200'
      });
      expect(sessionRepository.remove).to.have.been.calledWith('SESSION_ID');
    });
  });

});
