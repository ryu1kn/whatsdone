
import LambdaRequestNormaliser = require('../lib/LambdaRequestNormaliser');
import {expect} from './TestUtils';

describe('Server LambdaRequestNormaliser', () => {

  it('gives you request path', () => {
    const normaliser = new LambdaRequestNormaliser();
    const lambdaEvent = {
      path: 'PATH',
      headers: {},
      requestContext: {
        authorizer: {claims: 'CLAIMS'}
      }
    };
    expect(normaliser.normalise(lambdaEvent)).to.include({
      path: 'PATH'
    });
  });

  it('gives you path parameters', () => {
    const normaliser = new LambdaRequestNormaliser();
    const lambdaEvent = {
      pathParameters: 'PATH_PARAMS',
      headers: {},
      requestContext: {
        authorizer: {claims: 'CLAIMS'}
      }
    };
    expect(normaliser.normalise(lambdaEvent)).to.include({
      params: 'PATH_PARAMS'
    });
  });

  it('gives you query parameters', () => {
    const normaliser = new LambdaRequestNormaliser();
    const lambdaEvent = {
      queryStringParameters: 'QUERY_PARAMS',
      headers: {},
      requestContext: {
        authorizer: {claims: 'CLAIMS'}
      }
    };
    expect(normaliser.normalise(lambdaEvent)).to.include({
      query: 'QUERY_PARAMS'
    });
  });

  it('gives you an empty object for query parameters if it is not given', () => {
    const normaliser = new LambdaRequestNormaliser();
    const lambdaEvent = {
      headers: {},
      requestContext: {
        authorizer: {claims: 'CLAIMS'}
      }
    };
    expect(normaliser.normalise(lambdaEvent).query).to.include({});
  });

  it('parses the request body as json string if content-type is application/json', () => {
    const normaliser = new LambdaRequestNormaliser();
    const lambdaEvent = {
      body: '{"KEY":"VALUE"}',
      headers: {
        'Content-Type': 'application/json'
      },
      requestContext: {
        authorizer: {claims: 'CLAIMS'}
      }
    };
    expect(normaliser.normalise(lambdaEvent).body).to.include({
      KEY: 'VALUE'
    });
  });

  it('parses the request body as query string if content-type is not application/json', () => {
    const normaliser = new LambdaRequestNormaliser();
    const lambdaEvent = {
      body: 'KEY=VALUE',
      headers: {},
      requestContext: {
        authorizer: {claims: 'CLAIMS'}
      }
    };
    expect(normaliser.normalise(lambdaEvent).body).to.include({
      KEY: 'VALUE'
    });
  });

});
