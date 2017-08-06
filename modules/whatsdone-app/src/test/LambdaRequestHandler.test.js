
const LambdaRequestHandler = require('../lib/LambdaRequestHandler');
const ServiceLocator = require('../lib/ServiceLocator');

describe('Server LambdaRequestHandler', () => {

  it('bridges express req/res world to express agnostic http request processor', () => {
    const requestNormaliser = {normalise: sinon.stub().returns({
      REQUEST_DATA: '..',
      sessionId: 'SESSION_ID'
    })};
    const sessionRepository = {getById: sinon.stub().returns(Promise.resolve('SESSION'))};
    const responseFormatter = {format: sinon.stub().returns('LAMBDA_RESPONSE')};
    const requestProcessor = {process: sinon.stub().returns(Promise.resolve('RESPONSE'))};
    ServiceLocator.load({
      createLambdaRequestNormaliser: () => requestNormaliser,
      createSessionRepository: () => sessionRepository,
      createLambdaResponseFormatter: () => responseFormatter,
      createRequestProcessErrorProcessor: () => {},
      createAuthBasedRedirector: () => ({redirect: () => {}})
    });
    const lambdaCallback = sinon.spy();
    const handler = new LambdaRequestHandler({requestProcessor});

    return handler.handle('LAMBDA_EVENT', 'LAMBDA_CONTEXT', lambdaCallback).then(() => {
      expect(requestNormaliser.normalise).to.have.been.calledWith('LAMBDA_EVENT');
      expect(sessionRepository.getById).to.have.been.calledWith('SESSION_ID');
      expect(requestProcessor.process).to.have.been.calledWith(
        {REQUEST_DATA: '..', sessionId: 'SESSION_ID'},
        'SESSION'
      );
      expect(responseFormatter.format).to.have.been.calledWith('RESPONSE');
      expect(lambdaCallback).to.have.been.calledWith(null, 'LAMBDA_RESPONSE');
    });
  });

  it('does not try to load session if session id is not available', () => {
    const requestNormaliser = {normalise: sinon.stub().returns({
      REQUEST_DATA: '..',
      sessionId: null
    })};
    const sessionRepository = {getById: sinon.spy()};
    const responseFormatter = {format: sinon.stub().returns('LAMBDA_RESPONSE')};
    const requestProcessor = {process: sinon.stub().returns(Promise.resolve('RESPONSE'))};
    ServiceLocator.load({
      createLambdaRequestNormaliser: () => requestNormaliser,
      createSessionRepository: () => sessionRepository,
      createLambdaResponseFormatter: () => responseFormatter,
      createRequestProcessErrorProcessor: () => {},
      createAuthBasedRedirector: () => ({redirect: () => {}})
    });
    const lambdaCallback = sinon.spy();
    const handler = new LambdaRequestHandler({requestProcessor});

    return handler.handle('LAMBDA_EVENT', 'LAMBDA_CONTEXT', lambdaCallback).then(() => {
      expect(sessionRepository.getById).to.have.been.not.called;
      expect(requestProcessor.process).to.have.been.calledWith({REQUEST_DATA: '..', sessionId: null});
      expect(responseFormatter.format).to.have.been.calledWith('RESPONSE');
      expect(lambdaCallback).to.have.been.calledWith(null, 'LAMBDA_RESPONSE');
    });
  });

  it('catches an exception occurred during request process step', () => {
    const requestNormaliser = {normalise: () => 'NORMALISED_REQUEST'};
    const responseFormatter = {format: sinon.stub().returns('LAMBDA_RESPONSE')};
    const requestProcessor = {process: () => Promise.reject(new Error('UNEXPECTED_ERROR'))};
    const requestProcessErrorProcessor = {process: sinon.stub().returns('ERROR_RESPONSE')};
    ServiceLocator.load({
      createLambdaRequestNormaliser: () => requestNormaliser,
      createSessionRepository: () => ({getById: () => Promise.resolve()}),
      createLambdaResponseFormatter: () => responseFormatter,
      createRequestProcessErrorProcessor: () => requestProcessErrorProcessor,
      createAuthBasedRedirector: () => ({redirect: () => {}})
    });
    const lambdaCallback = sinon.spy();
    const handler = new LambdaRequestHandler({requestProcessor});

    return handler.handle('LAMBDA_EVENT', 'LAMBDA_CONTEXT', lambdaCallback).then(() => {
      expect(requestProcessErrorProcessor.process.args[0][0]).to.have.property('message', 'UNEXPECTED_ERROR');
      expect(responseFormatter.format).to.have.been.calledWith('ERROR_RESPONSE');
      expect(lambdaCallback).to.have.been.calledWith(null, 'LAMBDA_RESPONSE');
    });
  });

  it('redirects depending on the authentication status', () => {
    const requestNormaliser = {normalise: sinon.stub().returns('NORMALISED_REQUEST')};
    const responseFormatter = {format: sinon.stub().returns('LAMBDA_RESPONSE')};
    const requestProcessor = {process: sinon.spy()};
    const authBasedRedirector = {redirect: sinon.stub().returns('REDIRECT_RESPONSE')};
    ServiceLocator.load({
      createLambdaRequestNormaliser: () => requestNormaliser,
      createSessionRepository: () => ({getById: () => Promise.resolve()}),
      createLambdaResponseFormatter: () => responseFormatter,
      createRequestProcessErrorProcessor: () => {},
      createAuthBasedRedirector: () => authBasedRedirector
    });
    const lambdaCallback = sinon.spy();
    const handler = new LambdaRequestHandler({requestProcessor});

    return handler.handle('LAMBDA_EVENT', 'LAMBDA_CONTEXT', lambdaCallback).then(() => {
      expect(requestNormaliser.normalise).to.have.been.calledWith('LAMBDA_EVENT');
      expect(authBasedRedirector.redirect).to.have.been.calledWith('NORMALISED_REQUEST');
      expect(requestProcessor.process).to.not.have.been.called;
      expect(responseFormatter.format).to.have.been.calledWith('REDIRECT_RESPONSE');
      expect(lambdaCallback).to.have.been.calledWith(null, 'LAMBDA_RESPONSE');
    });
  });

});
