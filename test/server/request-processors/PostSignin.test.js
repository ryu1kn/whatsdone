
const PostSigninRequestProcessor = require('../../../src/server/request-processors/PostSignin');
const ServiceLocator = require('../../../src/server/ServiceLocator');

describe('Server PostSigninRequestProcessor', () => {

  it('takes user to the root page and update session info if the user exists', () => {
    const userRepository = {
      findUser: sinon.stub().returns(Promise.resolve({id: 'USER_ID'}))
    };
    ServiceLocator.load({
      createUserRepository: () => userRepository,
      createHtmlPageGenerator: () => {}
    });
    const processor = new PostSigninRequestProcessor();

    const request = {
      body: {
        email: 'EMAIL',
        password: 'PASSWORD'
      },
      session: {}
    };
    return processor.process(request).then(result => {
      expect(result).to.eql({
        statusCode: '303',
        headers: {Location: '/'}
      });
      expect(request.session).to.eql({
        isAuthorized: true,
        userId: 'USER_ID'
      });
      expect(userRepository.findUser).to.have.been.calledWith({
        email: 'EMAIL',
        password: 'PASSWORD'
      });
    });
  });

  it('still shows signin page', () => {
    const htmlPageGenerator = {generate: sinon.stub().returns('HTML')};
    ServiceLocator.load({
      createUserRepository: () => ({
        findUser: () => Promise.resolve(null)
      }),
      createHtmlPageGenerator: () => htmlPageGenerator
    });
    const processor = new PostSigninRequestProcessor();

    const request = {session: {}};
    return processor.process(request).then(result => {
      expect(result).to.eql({
        statusCode: '401',
        headers: {'Content-Type': 'text/html'},
        body: 'HTML'
      });
      expect(htmlPageGenerator.generate).to.have.been.calledWith('signin');
      expect(request.session).to.eql({});
    });
  });

  it('propagates error', () => {
    ServiceLocator.load({
      createUserRepository: () => ({
        findUser: () => Promise.reject(new Error('UNEXPECTED_ERROR'))
      }),
      createHtmlPageGenerator: () => {}
    });
    const processor = new PostSigninRequestProcessor();

    const request = {session: {}};
    return processor.process(request).then(
      throwError,
      e => {
        expect(e).to.have.property('message', 'UNEXPECTED_ERROR');
      }
    );
  });

});

