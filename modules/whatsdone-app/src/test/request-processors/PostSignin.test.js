
const PostSigninRequestProcessor = require('../../lib/request-processors/PostSignin');
const ServiceLocator = require('../../lib/ServiceLocator');

describe('Server PostSigninRequestProcessor', () => {

  it('takes user to the root page and update session info if the user exists', () => {
    const loginCommand = {execute: sinon.stub().returns(Promise.resolve('SESSION_ID'))};
    const cookieCodec = {encode: sinon.stub().returns('SESSION_EMBEDDED_COOKIE')};
    ServiceLocator.load({
      createLoginCommand: () => loginCommand,
      createHtmlPageGenerator: () => {},
      createCookieCodec: () => cookieCodec
    });
    const processor = new PostSigninRequestProcessor();

    const request = {
      body: {
        email: 'EMAIL',
        password: 'PASSWORD'
      }
    };
    return processor.process(request).then(result => {
      expect(result).to.eql({
        statusCode: '303',
        headers: {
          Location: '/',
          'Set-cookie': 'SESSION_EMBEDDED_COOKIE'
        }
      });
      expect(loginCommand.execute).to.have.been.calledWith({
        email: 'EMAIL',
        password: 'PASSWORD'
      });
      expect(cookieCodec.encode).to.have.been.calledWith({
        sessionId: 'SESSION_ID'
      });
    });
  });

  it('still shows signin page', () => {
    const htmlPageGenerator = {generate: sinon.stub().returns('HTML')};
    ServiceLocator.load({
      createLoginCommand: () => ({
        execute: () => Promise.resolve(null)
      }),
      createHtmlPageGenerator: () => htmlPageGenerator,
      createCookieCodec: () => {}
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
    });
  });

  it('propagates error', () => {
    ServiceLocator.load({
      createLoginCommand: () => ({
        execute: () => Promise.reject(new Error('UNEXPECTED_ERROR'))
      }),
      createHtmlPageGenerator: () => {},
      createCookieCodec: () => {}
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

