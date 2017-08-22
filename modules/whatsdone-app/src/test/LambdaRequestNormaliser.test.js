
const ServiceLocator = require('../lib/ServiceLocator');
const LambdaRequestNormaliser = require('../lib/LambdaRequestNormaliser');

describe('Server LambdaRequestNormaliser', () => {

  it('gives you request path', () => {
    const cookieCodec = {extractSessionId: () => {}};
    ServiceLocator.load({createCookieCodec: () => cookieCodec});
    const normaliser = new LambdaRequestNormaliser();
    const lambdaEvent = {
      path: 'PATH',
      headers: {}
    };
    expect(normaliser.normalise(lambdaEvent)).to.include({
      path: 'PATH'
    });
  });

  it('gives you path parameters', () => {
    const cookieCodec = {extractSessionId: () => {}};
    ServiceLocator.load({createCookieCodec: () => cookieCodec});
    const normaliser = new LambdaRequestNormaliser();
    const lambdaEvent = {
      pathParameters: 'PATH_PARAMS',
      headers: {}
    };
    expect(normaliser.normalise(lambdaEvent)).to.include({
      params: 'PATH_PARAMS'
    });
  });

  it('gives you query parameters', () => {
    const cookieCodec = {extractSessionId: () => {}};
    ServiceLocator.load({createCookieCodec: () => cookieCodec});
    const normaliser = new LambdaRequestNormaliser();
    const lambdaEvent = {
      queryStringParameters: 'QUERY_PARAMS',
      headers: {}
    };
    expect(normaliser.normalise(lambdaEvent)).to.include({
      query: 'QUERY_PARAMS'
    });
  });

  it('gives you an empty object for query parameters if it is not given', () => {
    const cookieCodec = {extractSessionId: () => {}};
    ServiceLocator.load({createCookieCodec: () => cookieCodec});
    const normaliser = new LambdaRequestNormaliser();
    const lambdaEvent = {
      headers: {}
    };
    expect(normaliser.normalise(lambdaEvent).query).to.include({});
  });

  it('parses the request body as json string if content-type is application/json', () => {
    const cookieCodec = {extractSessionId: () => {}};
    ServiceLocator.load({createCookieCodec: () => cookieCodec});
    const normaliser = new LambdaRequestNormaliser();
    const lambdaEvent = {
      body: '{"KEY":"VALUE"}',
      headers: {
        'Content-Type': 'application/json'
      }
    };
    expect(normaliser.normalise(lambdaEvent).body).to.include({
      KEY: 'VALUE'
    });
  });

  it('parses the request body as query string if content-type is not application/json', () => {
    const cookieCodec = {extractSessionId: () => {}};
    ServiceLocator.load({createCookieCodec: () => cookieCodec});
    const normaliser = new LambdaRequestNormaliser();
    const lambdaEvent = {
      body: 'KEY=VALUE',
      headers: {}
    };
    expect(normaliser.normalise(lambdaEvent).body).to.include({
      KEY: 'VALUE'
    });
  });

  it('extracts session id from cookie', () => {
    const cookieCodec = {extractSessionId: sinon.stub().returns('PARSED_COOKIE')};
    ServiceLocator.load({createCookieCodec: () => cookieCodec});
    const normaliser = new LambdaRequestNormaliser();
    const lambdaEvent = {
      headers: {
        Cookie: 'COOKIE'
      }
    };
    expect(normaliser.normalise(lambdaEvent)).to.include({
      sessionId: 'PARSED_COOKIE'
    });
    expect(cookieCodec.extractSessionId).to.have.been.calledWith('COOKIE');
  });

});
