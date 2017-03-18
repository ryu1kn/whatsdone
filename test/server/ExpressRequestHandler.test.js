
const ExpressRequestHandler = require('../../src/server/ExpressRequestHandler');
const ServiceLocator = require('../../src/server/ServiceLocator');

describe('Server ExpressRequestHandler', () => {

  it('bridges express req/res world to express agnostic http request processor', () => {
    const requestNormaliser = {normalise: sinon.stub().returns('NORMALISED_REQUEST')};
    const responseSender = {send: sinon.spy()};
    const responseSenderFactory = {create: sinon.stub().returns(responseSender)};
    const requestProcessor = {process: sinon.stub().returns(Promise.resolve('RESPONSE'))};
    ServiceLocator.load({
      createExpressRequestNormaliser: () => requestNormaliser,
      createExpressResponseSenderFactory: () => responseSenderFactory,
      createRequestProcessErrorProcessor: () => {},
      createAuthBasedRedirector: () => ({redirect: () => {}})
    });
    const handler = new ExpressRequestHandler({requestProcessor});

    return handler.handle('EXPRESS_REQ', 'EXPRESS_RES').then(() => {
      expect(requestNormaliser.normalise).to.have.been.calledWith('EXPRESS_REQ');
      expect(requestProcessor.process).to.have.been.calledWith('NORMALISED_REQUEST');
      expect(responseSenderFactory.create).to.have.been.calledWith('EXPRESS_RES');
      expect(responseSender.send).to.have.been.calledWith('RESPONSE');
    });
  });

  it('handles response data given both in sync/async way', () => {
    const requestNormaliser = {normalise: () => 'NORMALISED_REQUEST'};
    const responseSender = {send: sinon.spy()};
    const responseSenderFactory = {create: () => responseSender};
    const requestProcessor = {process: () => 'RESPONSE'};
    ServiceLocator.load({
      createExpressRequestNormaliser: () => requestNormaliser,
      createExpressResponseSenderFactory: () => responseSenderFactory,
      createRequestProcessErrorProcessor: () => {},
      createAuthBasedRedirector: () => ({redirect: () => {}})
    });
    const handler = new ExpressRequestHandler({requestProcessor});

    return handler.handle('EXPRESS_REQ', 'EXPRESS_RES').then(() => {
      expect(responseSender.send).to.have.been.calledWith('RESPONSE');
    });
  });

  it('catches an exception occurred during request process step', () => {
    const requestNormaliser = {normalise: () => 'NORMALISED_REQUEST'};
    const responseSender = {send: sinon.spy()};
    const responseSenderFactory = {create: () => responseSender};
    const requestProcessor = {process: () => Promise.reject(new Error('UNEXPECTED_ERROR'))};
    const requestProcessErrorProcessor = {process: sinon.stub().returns('ERROR_RESPONSE')};
    ServiceLocator.load({
      createExpressRequestNormaliser: () => requestNormaliser,
      createExpressResponseSenderFactory: () => responseSenderFactory,
      createRequestProcessErrorProcessor: () => requestProcessErrorProcessor,
      createAuthBasedRedirector: () => ({redirect: () => {}})
    });
    const handler = new ExpressRequestHandler({requestProcessor});

    return handler.handle('EXPRESS_REQ', 'EXPRESS_RES').then(() => {
      expect(responseSender.send).to.have.been.calledWith('ERROR_RESPONSE');
      expect(requestProcessErrorProcessor.process.args[0][0]).to.have.property('message', 'UNEXPECTED_ERROR');
    });
  });

  it('redirects depending on the authentication status', () => {
    const requestNormaliser = {normalise: sinon.stub().returns('NORMALISED_REQUEST')};
    const responseSender = {send: sinon.spy()};
    const responseSenderFactory = {create: sinon.stub().returns(responseSender)};
    const requestProcessor = {process: sinon.spy()};
    const authBasedRedirector = {redirect: sinon.stub().returns('REDIRECT_RESPONSE')};
    ServiceLocator.load({
      createExpressRequestNormaliser: () => requestNormaliser,
      createExpressResponseSenderFactory: () => responseSenderFactory,
      createRequestProcessErrorProcessor: () => {},
      createAuthBasedRedirector: () => authBasedRedirector
    });
    const handler = new ExpressRequestHandler({requestProcessor});

    return handler.handle('EXPRESS_REQ', 'EXPRESS_RES').then(() => {
      expect(requestNormaliser.normalise).to.have.been.calledWith('EXPRESS_REQ');
      expect(authBasedRedirector.redirect).to.have.been.calledWith('NORMALISED_REQUEST');
      expect(requestProcessor.process).to.not.have.been.called;
      expect(responseSender.send).to.have.been.calledWith('REDIRECT_RESPONSE');
    });
  });

});
