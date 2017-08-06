
const ServiceLocator = require('../lib/ServiceLocator');
const LambdaRequestNormaliser = require('../lib/LambdaRequestNormaliser');

describe('Server LambdaRequestNormaliser', () => {

  it('extracts http request information as a normalised form', () => {
    const cookieCodec = {extractSessionId: sinon.stub().returns('PARSED_COOKIE')};
    ServiceLocator.load({createCookieCodec: () => cookieCodec});
    const normaliser = new LambdaRequestNormaliser();
    const lambdaEvent = {
      path: 'PATH',
      pathParameters: 'PATH_PARAMETERS',
      body: 'KEY=VALUE',
      headers: {
        Cookie: 'COOKIE'
      }
    };
    expect(normaliser.normalise(lambdaEvent)).to.eql({
      path: 'PATH',
      params: 'PATH_PARAMETERS',
      body: {
        KEY: 'VALUE'
      },
      sessionId: 'PARSED_COOKIE'
    });
    expect(cookieCodec.extractSessionId).to.have.been.calledWith('COOKIE');
  });

  it('parses the request body as json string if content-type is application/json', () => {
    const cookieCodec = {extractSessionId: () => null};
    ServiceLocator.load({createCookieCodec: () => cookieCodec});
    const normaliser = new LambdaRequestNormaliser();
    const lambdaEvent = {
      path: 'PATH',
      pathParameters: 'PATH_PARAMETERS',
      body: '{"KEY":"VALUE"}',
      headers: {
        'Content-Type': 'application/json'
      }
    };
    expect(normaliser.normalise(lambdaEvent)).to.eql({
      path: 'PATH',
      params: 'PATH_PARAMETERS',
      body: {
        KEY: 'VALUE'
      },
      sessionId: null
    });
  });

});
