import LambdaRequestHandler from '../lib/LambdaRequestHandler';
import ServiceLocator from '../lib/ServiceLocator';
import {expect} from 'chai';
import {Event} from '../lib/models/Lambda';
import ServiceFactory from '../lib/ServiceFactory';
import {deepStrictEqual} from 'assert';
import sinon = require('sinon');

describe('Server LambdaRequestHandler', () => {
  const lambdaEvent: Event = {
    httpMethod: 'HTTP_METHOD',
    path: '/PATH',
    pathParameters: {},
    headers: {},
    requestContext: {
      authorizer: {
        claims: {
          'cognito:username': 'USERNAME',
          sub: 'SUB'
        }
      }
    }
  };

  it('bridges lambda request/response world to lambda agnostic http request processor', async () => {
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
    } as ServiceFactory);
    const handler = new LambdaRequestHandler(requestProcessor);

    const response = await handler.handle(lambdaEvent);
    deepStrictEqual(requestNormaliser.normalise.args[0], [lambdaEvent]);
    deepStrictEqual(userIdRepository.getByCognitoUserId.args[0], ['COGNITO_USER_ID']);
    deepStrictEqual(requestProcessor.process.args[0], [
      normalisedRequest,
      {
        userId: 'USER_ID',
        username: 'COGNITO_USER_NAME'
      }
    ]);
    deepStrictEqual(responseFormatter.format.args[0], ['RESPONSE']);
    deepStrictEqual(response, 'LAMBDA_RESPONSE');
  });

  it('catches an exception occurred during request process step', async () => {
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
    } as ServiceFactory);
    const handler = new LambdaRequestHandler(requestProcessor);

    const response = await handler.handle(lambdaEvent);
    expect(requestProcessErrorProcessor.process.args[0][0]).to.have.property('message', 'UNEXPECTED_ERROR');
    deepStrictEqual(responseFormatter.format.args[0], ['ERROR_RESPONSE']);
    deepStrictEqual(response, 'LAMBDA_RESPONSE');
  });
});
