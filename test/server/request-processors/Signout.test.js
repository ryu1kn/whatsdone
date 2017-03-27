
const ServiceLocator = require('../../../src/server/ServiceLocator');
const SignoutRequestProcessor = require('../../../src/server/request-processors/Signout');

describe('Server SignoutRequestProcessor', () => {

  it('removes session and shows user the signin page', () => {
    const sessionRepository = {remove: sinon.stub().returns(Promise.resolve())};
    ServiceLocator.load({createSessionRepository: () => sessionRepository});
    const processor = new SignoutRequestProcessor();
    const request = {};
    const session = {id: 'SESSION_ID'};

    return processor.process(request, session).then(response => {
      expect(response).to.eql({
        statusCode: '303',
        headers: {
          Location: '/signin'
        }
      });
      expect(sessionRepository.remove).to.have.been.calledWith('SESSION_ID');
    });
  });

});
