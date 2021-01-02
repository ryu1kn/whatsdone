import LambdaRequestNormaliser from '../lib/LambdaRequestNormaliser';
import {Event} from '../lib/models/Lambda';
import {deepStrictEqual} from 'assert';

describe('Server LambdaRequestNormaliser', () => {
  const normaliser = new LambdaRequestNormaliser();
  const lambdaEvent: Event = {
    httpMethod: 'HTTP_METHOD',
    path: 'PATH',
    pathParameters: {PATH_PARAM_KEY: 'PATH_PARAM_VALUE'},
    queryStringParameters: {QUERY_PARAM_KEY: 'QUERY_PARAM_VALUE'},
    body: '{"KEY":"VALUE"}',
    headers: {
      'Content-Type': 'application/json'
    },
    requestContext: {
      authorizer: {
        claims: {
          'cognito:username': 'USERNAME',
          sub: 'SUB'
        }
      }
    }
  };

  it('gives you request path', () => {
    deepStrictEqual(normaliser.normalise(lambdaEvent).path, 'PATH');
  });

  it('gives you path parameters', () => {
    deepStrictEqual(normaliser.normalise(lambdaEvent).params, {
      PATH_PARAM_KEY: 'PATH_PARAM_VALUE'
    });
  });

  it('gives you query parameters', () => {
    deepStrictEqual(normaliser.normalise(lambdaEvent).query.QUERY_PARAM_KEY, 'QUERY_PARAM_VALUE');
  });

  it('gives you an empty object for query parameters if it is not given', () => {
    const event = removeProperty(lambdaEvent, 'queryStringParameters');
    deepStrictEqual(normaliser.normalise(event).query, {});
  });

  it('parses the request body as json string if content-type is application/json', () => {
    deepStrictEqual(normaliser.normalise(lambdaEvent).body, {KEY: 'VALUE'});
  });

  it('parses the request body as query string if content-type is not application/json', () => {
    const normaliser = new LambdaRequestNormaliser();
    const overwrite = {
      headers: {},
      body: 'KEY=VALUE'
    };
    const event = Object.assign({}, lambdaEvent, overwrite);
    deepStrictEqual(normaliser.normalise(event).body, Object.assign(Object.create(null), {KEY: 'VALUE'}));
  });

  function removeProperty(event: Event, propertyKey: string) {
    return Object.entries(event).reduce((newObject, [key, value]) =>
      key === propertyKey ? newObject : Object.assign({}, newObject, {[key]: value}),
      {}
    ) as Event;
  }
});
