import LambdaRequestHandler from '../lib/LambdaRequestHandler';
import ServiceLocator from '../lib/ServiceLocator';
import {Event} from '../lib/models/Lambda';
import ServiceFactory from '../lib/ServiceFactory';
import {deepStrictEqual} from 'assert';
import * as td from 'testdouble';
import LambdaRequestNormaliser, {Request} from '../lib/LambdaRequestNormaliser';
import UserIdRepository from '../lib/repositories/UserId';
import LambdaResponseFormatter from '../lib/LambdaResponseFormatter';
import {Response} from '../lib/models/Request';
import {RequestProcessor} from '../lib/RequestProcessor';
import RequestProcessErrorProcessor from '../lib/RequestProcessErrorProcessor';

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
  const lambdaEventTriggerError: Event = {...lambdaEvent, path: '/TRIGGER_ERROR'};

  // TODO: Use valid-type data to remove the cast
  const normalisedRequest = {
    REQUEST_DATA: '..',
    userInfo: {
      userId: 'COGNITO_USER_ID',
      username: 'COGNITO_USER_NAME'
    }
  } as unknown as Request;
  const normalisedRequestTriggerError = {...normalisedRequest, REQUEST_DATA: 'WILL_TRIGGER_ERROR'} as unknown as Request;

  const error = new Error('UNEXPECTED_ERROR');

  const requestNormaliser = td.instance(LambdaRequestNormaliser);
  td.when(requestNormaliser.normalise(lambdaEvent)).thenReturn(normalisedRequest);
  td.when(requestNormaliser.normalise(lambdaEventTriggerError))
    .thenReturn(normalisedRequestTriggerError);

  const userIdRepository = td.instance(UserIdRepository);
  td.when(userIdRepository.getByCognitoUserId('COGNITO_USER_ID')).thenResolve('USER_ID');

  const responseFormatter = td.instance(LambdaResponseFormatter);
  td.when(responseFormatter.format('RESPONSE' as unknown as Response)).thenReturn('LAMBDA_RESPONSE' as any);
  td.when(responseFormatter.format('ERROR_RESPONSE' as unknown as Response)).thenReturn('LAMBDA_RESPONSE_FOR_ERROR' as any);

  const requestProcessor = td.object<RequestProcessor>();
  td.when(requestProcessor.process(
    normalisedRequest,
    {userId: 'USER_ID', username: 'COGNITO_USER_NAME'}
  )).thenResolve('RESPONSE' as any);
  td.when(requestProcessor.process(
    normalisedRequestTriggerError,
    {userId: 'USER_ID', username: 'COGNITO_USER_NAME'}
  )).thenReject(error);

  const requestProcessErrorProcessor = td.instance(RequestProcessErrorProcessor);
  td.when(requestProcessErrorProcessor.process(error)).thenReturn('ERROR_RESPONSE' as any);

  ServiceLocator.load({
    createLambdaRequestNormaliser: () => requestNormaliser,
    createUserIdRepository: () => userIdRepository,
    createLambdaResponseFormatter: () => responseFormatter,
    createRequestProcessErrorProcessor: () => requestProcessErrorProcessor
  } as ServiceFactory);

  const handler = new LambdaRequestHandler(requestProcessor);

  it('bridges lambda request/response world to lambda agnostic http request processor', async () => {
    const response = await handler.handle(lambdaEvent);

    deepStrictEqual(response, 'LAMBDA_RESPONSE');
  });

  it('catches an exception occurred during request process step', async () => {
    const response = await handler.handle(lambdaEventTriggerError);

    deepStrictEqual(response, 'LAMBDA_RESPONSE_FOR_ERROR');
  });
});
