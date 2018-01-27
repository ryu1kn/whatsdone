
const LambdaRequestHandler = require('../lib/LambdaRequestHandler');
const ServiceLocator = require('../lib/ServiceLocator');

describe('Server LambdaRequestHandler', () => {

  it('bridges lambda request/response world to lambda agnostic http request processor', () => {
    const normalisedRequest = {
      REQUEST_DATA: '..',
      userInfo: {
        userId: 'COGNITO_USER_ID',
        username: 'COGNITO_USER_NAME'
      }
    };
    const requestNormaliser = {normalise: sinon.stub().returns(normalisedRequest)};
    const userIdRepository = {getByCognitoUserId: sinon.stub().returns(Promise.resolve('USER_ID'))};
    const responseFormatter = {format: sinon.stub().returns('LAMBDA_RESPONSE')};
    const requestProcessor = {process: sinon.stub().returns(Promise.resolve('RESPONSE'))};
    ServiceLocator.load({
      createLambdaRequestNormaliser: () => requestNormaliser,
      createUserIdRepository: () => userIdRepository,
      createLambdaResponseFormatter: () => responseFormatter,
      createRequestProcessErrorProcessor: () => {}
    });
    const lambdaCallback = sinon.spy();
    const handler = new LambdaRequestHandler({requestProcessor});

    return handler.handle('LAMBDA_EVENT', 'LAMBDA_CONTEXT', lambdaCallback).then(() => {
      expect(requestNormaliser.normalise).to.have.been.calledWith('LAMBDA_EVENT');
      expect(userIdRepository.getByCognitoUserId).to.have.been.calledWith('COGNITO_USER_ID');
      expect(requestProcessor.process).to.have.been.calledWith(
        normalisedRequest,
        {
          userId: 'USER_ID',
          username: 'COGNITO_USER_NAME'
        }
      );
      expect(responseFormatter.format).to.have.been.calledWith('RESPONSE');
      expect(lambdaCallback).to.have.been.calledWith(null, 'LAMBDA_RESPONSE');
    });
  });

  it('catches an exception occurred during request process step', () => {
    const requestNormaliser = {normalise: () => ({
      REQUEST_DATA: '..',
      userInfo: {sub: 'COGNITO_USER_ID'}
    })};
    const responseFormatter = {format: sinon.stub().returns('LAMBDA_RESPONSE')};
    const requestProcessor = {process: () => Promise.reject(new Error('UNEXPECTED_ERROR'))};
    const requestProcessErrorProcessor = {process: sinon.stub().returns('ERROR_RESPONSE')};
    ServiceLocator.load({
      createLambdaRequestNormaliser: () => requestNormaliser,
      createUserIdRepository: () => ({getByCognitoUserId: () => Promise.resolve()}),
      createLambdaResponseFormatter: () => responseFormatter,
      createRequestProcessErrorProcessor: () => requestProcessErrorProcessor
    });
    const lambdaCallback = sinon.spy();
    const handler = new LambdaRequestHandler({requestProcessor});

    return handler.handle('LAMBDA_EVENT', 'LAMBDA_CONTEXT', lambdaCallback).then(() => {
      expect(requestProcessErrorProcessor.process.args[0][0]).to.have.property('message', 'UNEXPECTED_ERROR');
      expect(responseFormatter.format).to.have.been.calledWith('ERROR_RESPONSE');
      expect(lambdaCallback).to.have.been.calledWith(null, 'LAMBDA_RESPONSE');
    });
  });

});
